{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Driftwood where

import qualified Data.ByteString as B
import Data.Aeson
import Data.Aeson.Types (fieldLabelModifier, Options)
import GHC.Generics
import Data.Char (toLower)

data Message = Message
    { messageSrc  :: String
    , messageDest :: String
    , messageBody :: Body
    } deriving (Generic, Show, Eq)

data Body
    = RaftInit RaftInitBody
    | RaftInitOk Reply
    | Error ErrorBody
    | Write WriteBody
    | WriteOk Reply
    | Read Request
    | ReadOk ReadReply
    | Cas CasBody
    | CasOk Reply
    | Delete Request
    | DeleteOk Reply
    deriving (Generic, Show, Eq)

data RaftInitBody
    = RaftInitBody
    { raftInitBodyMsgId   :: Int
    , raftInitBodyNodeId  :: Int
    , raftInitBodyNodeIds :: [Int]
    } deriving (Generic, Show, Eq)

data Reply = Reply { replyInReplyTo :: Int } deriving (Generic, Show, Eq)

data Request
    = Request
    { requestMsgId :: Int
    , requestKey   :: String
    } deriving (Generic, Show, Eq)

data ErrorBody
    = ErrorBody
    { errorCode      :: Maybe Int
    , errorText      :: Maybe String
    , errorInReplyTo :: Int
    } deriving (Generic, Show, Eq)

data WriteBody
    = WriteBody
    { writeMsgId :: Int
    , writeKey   :: String
    , writeValue :: String
    } deriving (Generic, Show, Eq)

data ReadReply
    = ReadReply
    { readInReplyTo :: Int
    , readValue :: String
    } deriving (Generic, Show, Eq)

data CasBody
    = CasBody
    { casMsgId :: Int
    , casKey   :: String
    , casFrom  :: String
    , casTo    :: String
    } deriving (Generic, Show, Eq)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
