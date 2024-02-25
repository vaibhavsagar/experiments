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
    when (length args < 2) $ error "no chunkSize and filename provided"
    let chunkSize = read (head args) :: Int
    let filename = head (tail args)
    contents <- B.readFile filename
    let processed = processAll chunkSize contents
    let output = formatOutput processed
    TIO.putStrLn output
