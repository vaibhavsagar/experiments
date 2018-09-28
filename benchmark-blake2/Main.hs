{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

module Main where

import           "criterion"         Criterion.Main
import qualified "memory"            Data.ByteArray             as B
import qualified "base16-bytestring" Data.ByteString.Base16     as B16
import qualified "bytestring"        Data.ByteString            as BS
import qualified "blake2"            Crypto.Hash.BLAKE2.BLAKE2b as Original
import qualified "blake2-patched"    Crypto.Hash.BLAKE2.BLAKE2b as Patched
import qualified "cryptonite"        Crypto.Hash                as Cryptonite

cryptoniteHash :: BS.ByteString -> BS.ByteString
cryptoniteHash = B16.encode . B.convert . Cryptonite.hashWith Cryptonite.Blake2b_512
{-# INLINE cryptoniteHash #-}

originalHash :: BS.ByteString -> BS.ByteString
originalHash = Original.hash 64 mempty
{-# INLINE originalHash #-}

patchedHash :: BS.ByteString -> BS.ByteString
patchedHash = Patched.hash 64 mempty
{-# INLINE patchedHash #-}

main :: IO ()
main = defaultMain
  [ bgroup "blake2"
    [

    ]
  , bgroup "blake2-patched"
    [

    ]
  , bgroup "cryptonite"
    [

    ]
  ]
