module Main where

import Control.Monad (when)
import System.Environment

import Debug.Trace

import OneBRC

main :: IO ()
main = do
    args <- getArgs
    when (length args < 1) $ error "no filename provided"
    let filename = head args
    contents <- readFile filename
    traceIO $ "read contents"
    let processed = processAll contents
    traceIO $ "processed contents"
    let calculated = calculateAll processed
    traceIO $ "calculated contents"
    let formatted = formatMap calculated
    traceIO $ "formatted contents"
    let output = formatOutput formatted
    traceIO $ "formatted output"
    putStrLn output
