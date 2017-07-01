{-# LANGUAGE OverloadedStrings #-}

module Nix.Serve (module Nix.Serve) where

import Data.ByteString
import Network.HTTP.Types.Status
import Nix.LocalStore
import Nix.Store
import Web.Scotty

server :: ScottyM ()
server = do
    get "/nix-cache-info" $ do return ()
    get (regex "/([0-9a-z]+)\\.narinfo") $ do
        hashPart <- param "1" :: ActionM ByteString
        return ()
    get (regex "/nar/([0-9a-z]+)\\.nar.bz2") $ do
        hashPart <- param "1" :: ActionM ByteString
        return ()
    notFound $ status $ mkStatus 404 "File not found."
