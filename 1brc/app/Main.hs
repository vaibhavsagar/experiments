module Main where

import Control.Monad (when)
import qualified Data.Text.IO.Utf8 as TIO
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString as B
import System.Environment

import OneBRC

main :: IO ()
main = do
    args <- getArgs
    when (length args < 1) $ error "no chunkSize and filename provided"
    let filename = head args
    contents <- B.readFile filename
    processed <- processAll contents
    let output = formatOutput processed
    TIO.putStrLn output
