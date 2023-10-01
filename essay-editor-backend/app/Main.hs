{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.UTF8 as UTF8
import Data.IORef
import GHC.Generics
import Web.Scotty

data EssayState = EssayState
    { essayStateContents :: String
    , essayStateHash :: String
    }
    deriving (Eq, Show)

data EssayInitResponse = EssayInitResponse
    { essayInitResponseEssayBody :: String
    , essayInitResponseEssayHash :: String
    } deriving (Eq, Show, Generic, A.ToJSON)

data EssaySyncRequest = EssaySyncRequest
    { essaySyncRequestEssayBody :: String
    , essaySyncRequestPreviousHash :: String
    }
    deriving (Eq, Show, Generic, A.FromJSON)

data EssaySyncResponse = EssaySyncResponse
    { essaySyncResponseCurrentHash :: String
    , essaySyncResponseEssayIsCurrent :: Bool
    }
    deriving (Eq, Show, Generic, A.ToJSON)

main :: IO ()
main = do
    essayState <- newIORef $ EssayState "" (hashString "")
    scotty 4000 $ do
        get "/init" $ do
            EssayState essayContents essayHash <- liftIO $ readIORef essayState
            json $ EssayInitResponse essayContents essayHash
        post "/sync" $ do
            EssaySyncRequest essayBody previousHash <- jsonData
            response <- liftIO $ atomicModifyIORef' essayState $ \currentState@(EssayState contents contentsHash) ->
                if (previousHash == contentsHash)
                    then let
                        newHash = hashString essayBody
                        in (EssayState essayBody newHash, EssaySyncResponse newHash True)
                    else (currentState, EssaySyncResponse contentsHash False)
            json response

hashString :: String -> String
hashString string = let
    bytestring = UTF8.fromString string
    hashed = SHA1.hash bytestring
    base16 = B16.encode hashed
    in UTF8.toString base16
