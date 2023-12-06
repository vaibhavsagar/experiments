{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Driftwood where

import Data.Aeson
import Data.IORef
import Data.Maybe (fromJust)
import qualified Data.Text as T
import GHC.Generics
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified System.IO as IO

data Message = Message
    { src :: String
    , dest :: String
    , body :: Object
    } deriving (Eq, Show, Generic, FromJSON, ToJSON)

data InternalState = InternalState
    { internalStateNodeId :: String
    , internalStateMsgCounter :: Int
    } deriving (Eq, Show)

driftwood :: IO ()
driftwood = do
    state <- newIORef $ InternalState "" 0
    go state
    where
        go :: IORef InternalState -> IO ()
        go state = do
            parsed :: Maybe Message <- decodeStrict' <$> BC.getLine
            case parsed of
                Just msg -> do
                    IO.hPutStrLn IO.stderr $ "Received: " ++ show msg
                    case KM.lookup "type" (body msg) of
                        Just "init" -> case KM.lookup "node_id" (body msg) of
                            Just (String node_id) -> do
                                IO.hPutStrLn IO.stderr $ "Initialised node " ++ show node_id
                                _ <- atomicModifyIORef state $ \internalState ->
                                    (InternalState (T.unpack node_id) (internalStateMsgCounter internalState), ())
                                response <- reply state msg $ KM.insert "type" (String "init_ok") KM.empty
                                BL.putStr $ encode response
                                BL.putStr "\n"
                                IO.hFlush IO.stdout
                            Nothing -> error "node id not found"
                        Just "echo" -> do
                            IO.hPutStrLn IO.stderr $ "Echoing " ++ show (body msg)
                            response <- reply state msg $ KM.insert "type" (String "echo_ok") (body msg)
                            BL.putStr $ encode response
                            BL.putStr "\n"
                            IO.hFlush IO.stdout
                        _ -> error "did not understand message"
                    go state
                Nothing -> error "failed"
        reply :: IORef InternalState -> Message -> Object -> IO Message
        reply state request baseBody = do
            (msgId, nodeId) <- atomicModifyIORef state $ \internalState -> let
                counter' = internalStateMsgCounter internalState + 1
                internalState' = InternalState (internalStateNodeId internalState) counter'
                in (internalState', (counter', internalStateNodeId internalState))
            let responseBody =
                    KM.insert "msg_id" (Number (fromIntegral msgId)) $
                    KM.insert "in_reply_to" (fromJust $ KM.lookup "msg_id" (body request)) $
                    baseBody
            let message = Message (dest request) (src request) responseBody
            pure message
