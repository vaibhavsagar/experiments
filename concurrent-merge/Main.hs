#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell
#! nix-shell -p "haskellPackages.ghcWithPackages (p: [ p.base ])"

module Main where

import Control.Concurrent.MVar (MVar, newEmptyMVar, readMVar, takeMVar, putMVar)
import Control.Concurrent      (forkIO)
import Data.Maybe              (isJust, fromJust)
import Data.List               (minimumBy)
import Data.Ord                (comparing)

input1 = [ 1, 1, 1, 2 ]
input2 = [ 2, 3, 4, 4 ]

worker :: [Int] -> MVar (Maybe Int) -> IO ()
worker []     slot = putMVar slot Nothing
worker (x:xs) slot = putMVar slot (Just x) >> worker xs slot

merge :: [MVar (Maybe Int)] -> [Int] -> IO [Int]
merge slots output = traverse readMVar slots >>= \values ->
    case filter (isJust . snd) $ zip [0..] values of
        [] -> return output
        ls -> let
            (ix, Just el) = minimumBy (comparing (fromJust . snd)) ls
            in takeMVar (slots !! ix) >> merge slots (output ++ [el])

main :: IO ()
main = do
    slots <- sequenceA $ replicate 2 newEmptyMVar
    traverse (forkIO . uncurry worker) $ zip [input1, input2] slots
    print =<< merge slots []
