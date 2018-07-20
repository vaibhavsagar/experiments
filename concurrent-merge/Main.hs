#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell
#! nix-shell -p "haskellPackages.ghcWithPackages (p: [ p.base p.containers ])"

module Main where

import Control.Concurrent.MVar
import Control.Concurrent      (forkIO)
import Data.Maybe              (isJust, fromJust)
import Data.List               (minimumBy, maximumBy)
import Data.Ord                (Ordering, comparing)
import qualified Data.Sequence as Seq

input1 = [ 1, 1, 1, 5 ]
input2 = [ 2, 3, 4, 4 ]

frontWorker :: MVar (Seq.Seq Int) -> MVar (Maybe Int) -> IO ()
frontWorker queueVar slot = do
    queue <- takeMVar queueVar
    case queue of
        Seq.Empty   -> do
            putMVar queueVar Seq.Empty
            putMVar slot Nothing
        h Seq.:<| t -> do
            putMVar queueVar t
            putMVar slot (Just h)
            frontWorker queueVar slot

backWorker :: MVar (Seq.Seq Int) -> MVar (Maybe Int) -> IO ()
backWorker queueVar slot = do
    queue <- takeMVar queueVar
    case queue of
        Seq.Empty   -> do
            putMVar queueVar Seq.Empty
            putMVar slot Nothing
        i Seq.:|> l -> do
            putMVar queueVar i
            putMVar slot (Just l)
            backWorker queueVar slot

type Pair = (Int, Maybe Int)

merge
   :: ((Pair -> Pair -> Ordering)
   -> [Pair] -> Pair)
   -> [MVar (Maybe Int)]
   -> [Int]
   -> IO [Int]
merge cmp slots output = traverse readMVar slots >>= \values ->
    case filter (isJust . snd) $ zip [0..] values of
        [] -> return output
        ls -> let
            (ix, Just el) = cmp (comparing (fromJust . snd)) ls
            in takeMVar (slots !! ix) >> merge cmp slots (output ++ [el])

main :: IO ()
main = do
    frontSlots <- sequenceA $ replicate 2 newEmptyMVar
    backSlots  <- sequenceA $ replicate 2 newEmptyMVar
    i1 <- newMVar $ Seq.fromList input1
    i2 <- newMVar $ Seq.fromList input2
    traverse (forkIO . uncurry frontWorker) $ zip [i1, i2] frontSlots
    traverse (forkIO . uncurry backWorker)  $ zip [i1, i2] backSlots
    l1 <- merge minimumBy frontSlots []
    l2 <- merge maximumBy backSlots  []
    print $ l1 ++ (reverse l2)
