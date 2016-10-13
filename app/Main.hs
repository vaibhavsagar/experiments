module Main where

import Lib
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Distributed.Process
import Control.Distributed.Process.Node

main :: IO ()
main = do
    Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
    node    <- newLocalNode t initRemoteTable
    _       <- runProcess node $ do
        self  <- getSelfPid
        send self "hello"
        hello <- expect :: Process String
        liftIO $ putStrLn hello
    return ()
