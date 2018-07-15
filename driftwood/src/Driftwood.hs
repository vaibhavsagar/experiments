{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Driftwood where

import qualified Data.ByteString as B
import Data.Aeson
import Data.Monoid
import GHC.Generics
import Data.Char (toLower)

data Message = Message
    { messageSrc  :: String
    , messageDest :: String
    , messageBody :: Body
    } deriving (Generic, Show, Eq)

instance FromJSON Message where
    parseJSON = withObject "Message" $ \v -> Message
        <$> v .: "src"
        <*> v .: "dest"
        <*> v .: "body"

instance ToJSON Message where
    toJSON (Message src dest body)
        = object
        [ "src" .= src
        , "dest" .= dest
        , "body" .= body
        ]

    toEncoding (Message src dest body)
        = pairs
        (  "src"  .= src
        <> "dest" .= dest
        <> "body" .= body
        )

data Body
    = RaftInit RaftInitBody
    | RaftInitOk Reply
    | MError ErrorBody
    | Write WriteBody
    | WriteOk Reply
    | Read Request
    | ReadOk ReadReply
    | Cas CasBody
    | CasOk Reply
    | Delete Request
    | DeleteOk Reply
    deriving (Generic, Show, Eq)

instance FromJSON Body where
    parseJSON = withObject "Body" $ \v -> v .: "type" >>= \t ->
        case (t :: String) of
            "raft_init" -> fmap RaftInit $ RaftInitBody
                <$> v .: "msg_id"
                <*> v .: "node_id"
                <*> v .: "node_ids"
            "raft_init_ok" -> fmap RaftInitOk $ Reply
                <$> v .: "in_reply_to"
            "error" -> fmap MError $ ErrorBody
                <$> v .:? "code"
                <*> v .:? "text"
                <*> v .:  "in_reply_to"
            "write" -> fmap Write $ WriteBody
                <$> v .: "msg_id"
                <*> v .: "key"
                <*> v .: "value"
            "write_ok" -> fmap WriteOk $ Reply
                <$> v .: "in_reply_to"
            "read" -> fmap Read $ Request
                <$> v .: "msg_id"
                <*> v .: "key"
            "read_ok" -> fmap ReadOk $ ReadReply
                <$> v .: "in_reply_to"
                <*> v .: "value"
            "cas" -> fmap Cas $ CasBody
                <$> v .: "msg_id"
                <*> v .: "key"
                <*> v .: "from"
                <*> v .: "to"
            "cas_ok" -> fmap CasOk $ Reply
                <$> v .: "in_reply_to"
            "delete" -> fmap Delete $ Request
                <$> v .: "msg_id"
                <*> v .: "key"
            "delete_ok" -> fmap DeleteOk $ Reply
                <$> v .: "in_reply_to"

instance ToJSON Body where
    toJSON body = case body of
        RaftInit (RaftInitBody msg_id node_id node_ids) -> object
            [ "type"     .= String "raft_init"
            , "msg_id"   .= msg_id
            , "node_id"  .= node_id
            , "node_ids" .= node_ids
            ]
        RaftInitOk (Reply in_reply_to) -> object
            [ "type"        .= String "raft_init_ok"
            , "in_reply_to" .= in_reply_to
            ]
        MError (ErrorBody code text in_reply_to) -> object
            [ "type"        .= String "error"
            , "code"        .= code
            , "text"        .= text
            , "in_reply_to" .= in_reply_to
            ]
        Write (WriteBody msg_id key value) -> object
            [ "type"   .= String "write"
            , "msg_id" .= msg_id
            , "key"    .= key
            , "value"  .= value
            ]
        WriteOk (Reply in_reply_to) -> object
            [ "type"        .= String "write_ok"
            , "in_reply_to" .= in_reply_to
            ]
        Read (Request msg_id key) -> object
            [ "type"   .= String "read"
            , "msg_id" .= msg_id
            , "key"    .= key
            ]
        ReadOk (ReadReply in_reply_to value) -> object
            [ "type"        .= String "read_ok"
            , "in_reply_to" .= in_reply_to
            , "value"       .= value
            ]

data RaftInitBody
    = RaftInitBody
    { raftInitMsgId   :: Int
    , raftInitNodeId  :: Int
    , raftInitNodeIds :: [Int]
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
