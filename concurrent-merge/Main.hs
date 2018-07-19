#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell
#! nix-shell -p "haskellPackages.ghcWithPackages (p: [ p.base ])"

module Main where

import Control.Concurrent.MVar (MVar, newEmptyMVar, readMVar, takeMVar, putMVar)
import Control.Concurrent      (forkIO)
import Data.Maybe              (isJust)
import Data.List               (sortOn)


input1 = [ 1, 1, 1, 2 ]
input2 = [ 2, 3, 4, 4 ]

worker :: [Int] -> MVar (Maybe Int) -> IO ()
worker []     slot = putMVar slot Nothing
worker (x:xs) slot = do
    putMVar slot (Just x)
    worker xs slot

merge :: [MVar (Maybe Int)] -> [Int] -> IO [Int]
merge slots output = do
    values <- traverse readMVar slots
    let values' = zip [0..] values
    let filled = filter (\(_, i) -> isJust i) values'
    case filled of
        [] -> return output
        _  -> do
            let sorted = sortOn (\(_, Just i) -> i) filled
            let (idx, Just el) = head sorted
            let output' = output ++ [el]
            takeMVar (slots !! idx)
            merge slots output'

main :: IO ()
main = do
    slot1 <- newEmptyMVar
    slot2 <- newEmptyMVar
    forkIO $ worker input1 slot1
    forkIO $ worker input2 slot2
    result <- merge [slot1, slot2] []
    print result
