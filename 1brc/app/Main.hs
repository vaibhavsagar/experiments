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
    when (length args < 1) $ error "no filename provided"
    let filename = head args
    contents <- decodeUtf8 <$> B.readFile filename
    let processed = processAll contents
    let formatted = formatMap processed
    let output = formatOutput formatted
    TIO.putStrLn output
