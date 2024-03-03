{-# Language RankNTypes #-}
{-# Language BangPatterns #-}
{-# Language ScopedTypeVariables #-}

module Hashtable (new, insertWith, Hashtable.lookup, Hashtable.freeze, Hashtable(..), FrozenHashtable(..)) where

import Control.Monad.ST
import Data.Array
import Data.Array.Unsafe
import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Array.MArray
import Data.Array.ST
import Data.Bits
import qualified Data.ByteString as BS

data Hashtable s a = Hashtable
    { hashtableKeys :: {-# UNPACK #-} !(STArray s Int BS.ByteString)
    , hashtableValues :: {-# UNPACK #-} !(STArray s Int a)
    }

data FrozenHashtable a = FrozenHashtable
    { frozenHashtableKeys :: {-# UNPACK #-} !(Array Int BS.ByteString)
    , frozenHashtableValues :: {-# UNPACK #-} !(Array Int a)
    } deriving (Eq, Show)

lENGTH :: Int
lENGTH = 10007

freeze :: forall s a. Hashtable s a -> ST s (FrozenHashtable a)
freeze ht = do
    frozenKeys <- unsafeFreeze (hashtableKeys ht)
    frozenValues <- unsafeFreeze (hashtableValues ht)
    pure $ FrozenHashtable frozenKeys frozenValues

djb2 :: BS.ByteString -> Int
djb2 = BS.foldl' (\hash w -> (hash `shiftL` 5) + hash + (fromIntegral w)) 5381

new :: forall s a. ST s (Hashtable s a)
new = do
    keys <- newArray (0,lENGTH-1) BS.empty
    vals <- newArray_ (0,lENGTH-1)
    pure $ Hashtable keys vals

lookup :: forall a. BS.ByteString -> FrozenHashtable a -> a
lookup key ht = let
    startIdx = djb2 key `mod` lENGTH
    idx = {-# SCC lookupFindIndex #-} findIndex startIdx
    in (frozenHashtableValues ht) ! idx
    where
        findIndex idx = let
            foundKey = (frozenHashtableKeys ht) ! idx
            in case foundKey == key of
                True -> idx
                False -> findIndex ((idx+1) `rem` lENGTH)

insertWith :: forall s a. (a -> a -> a) -> BS.ByteString -> a -> Hashtable s a -> ST s Bool
insertWith f k v ht = do
    let startIdx = djb2 k `mod` lENGTH
    (idx, present) <- {-# SCC insertWithFindIndex #-} findIndex startIdx
    case present of
        True -> do
            el <- unsafeRead (hashtableValues ht) idx
            unsafeWrite (hashtableValues ht) idx $! f el v
            pure False
        False -> do
            unsafeWrite (hashtableKeys ht) idx k
            unsafeWrite (hashtableValues ht) idx v
            pure True
    where
        findIndex :: Int -> ST s (Int, Bool)
        findIndex idx = do
            foundKey <- unsafeRead (hashtableKeys ht) idx
            if foundKey == k
                then pure (idx, True)
                else if BS.null foundKey
                    then pure (idx, False)
                    else findIndex ((idx+1) `rem` lENGTH)
