{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Main (main) where

import           Data.Acid
import           Data.Acid.Remote
import           Data.Acid.Local (createCheckpointAndClose)

import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.ByteString (ByteString(..))
import           Data.ByteString.UTF8 (toString, fromString)
import           Data.SafeCopy
import           Network
import           System.Environment
import           System.Exit
import           System.IO

import           Data.Typeable

import qualified Data.Map             as Map

------------------------------------------------------
-- The Haskell structure that we want to encapsulate

type Key   = ByteString
type Value = ByteString

newtype KeyValue_v0 = KeyValue_v0 (Map.Map String String)
    deriving (Typeable)

newtype KeyValue = KeyValue (Map.Map Key Value)
    deriving (Typeable)

$(deriveSafeCopy 0 'base ''KeyValue_v0)
$(deriveSafeCopy 1 'extension ''KeyValue)

------------------------------------------------------
-- The transaction we will execute over the state.

insertKey_v0 :: String -> String -> Update KeyValue_v0 ()
insertKey_v0 key value
    = do KeyValue_v0 m <- get
         put (KeyValue_v0 (Map.insert key value m))

lookupKey_v0 :: String -> Query KeyValue_v0 (Maybe String)
lookupKey_v0 key
    = do KeyValue_v0 m <- ask
         return (Map.lookup key m)

$(makeAcidic ''KeyValue_v0 ['insertKey_v0, 'lookupKey_v0])

insertKey :: Key -> Value -> Update KeyValue ()
insertKey key value
    = do KeyValue m <- get
         put (KeyValue (Map.insert key value m))

lookupKey :: Key -> Query KeyValue (Maybe Value)
lookupKey key
    = do KeyValue m <- ask
         return (Map.lookup key m)

$(makeAcidic ''KeyValue ['insertKey, 'lookupKey])

instance Migrate KeyValue where
    type MigrateFrom KeyValue = KeyValue_v0
    migrate (KeyValue_v0 mp) = KeyValue $ Map.foldrWithKey (\k a -> Map.insert (fromString k) (fromString a)) Map.empty mp

------------------------------------------------------
-- This is how AcidState is used:

main :: IO ()
main = do args <- getArgs
          acid <- openLocalStateFrom "state/KeyValue" (KeyValue Map.empty)
          case args of
            [key]
              -> do mbKey <- query acid (LookupKey (fromString key))
                    case mbKey of
                      Nothing    -> putStrLn $ key ++ " has no associated value."
                      Just value -> putStrLn $ key ++ " = " ++ (toString value)
            [key,val]
              -> do update acid (InsertKey (fromString key) (fromString val))
                    putStrLn "Done."
            _ -> do putStrLn "Usage:"
                    putStrLn "  key               Lookup the value of 'key'."
                    putStrLn "  key value         Set the value of 'key' to 'value'."
          createCheckpointAndClose acid
