{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Driftwood where

import qualified Data.ByteString as B
import Data.Aeson
import Data.Aeson.Types (fieldLabelModifier, Options)
import GHC.Generics
import Data.Char (toLower)

data Missive = Missive
    { missiveSrc  :: String
    , missiveDest :: String
    , missiveBody :: Object
    } deriving (Generic, Show, Eq)

missiveOptions :: Options
missiveOptions = defaultOptions { fieldLabelModifier = modifyMissiveLabels }
    where modifyMissiveLabels = map toLower . drop 7

instance ToJSON Missive where
    toJSON = genericToJSON missiveOptions

instance FromJSON Missive where
    parseJSON = genericParseJSON missiveOptions

someFunc :: IO ()
someFunc = putStrLn "someFunc"


