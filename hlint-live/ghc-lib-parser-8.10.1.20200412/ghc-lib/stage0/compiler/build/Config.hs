{-# LANGUAGE CPP #-}
module Config
  ( module GHC.Version
  , cBuildPlatformString
  , cHostPlatformString
  , cProjectName
  , cBooterVersion
  , cStage
  ) where

import GhcPrelude

import GHC.Version

cBuildPlatformString :: String
cBuildPlatformString = "x86_64-apple-darwin"

cHostPlatformString :: String
cHostPlatformString = "x86_64-apple-darwin"

cProjectName          :: String
cProjectName          = "The Glorious Glasgow Haskell Compilation System"

cBooterVersion        :: String
cBooterVersion        = "8.6.5"

cStage                :: String
cStage                = show (1 :: Int)
