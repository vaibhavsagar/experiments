{-# Language RankNTypes #-}
{-# Language BangPatterns #-}
{-# Language ScopedTypeVariables #-}

module Hashtable (new, insertWith, Hashtable.lookup, Element(..), Pair(..)) where

import Control.Monad.ST
import Data.Array
import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Array.MArray
import Data.Array.ST
import Data.Bits
import qualified Data.List as List
import qualified Data.ByteString as BS

data Pair a = Pair
    { pairKey :: !BS.ByteString
    , pairValue :: !a
    } deriving (Eq, Show)

data Element a
    = Single (Pair a)
    | Collision [Pair a]
    | Empty
    deriving (Eq, Show)

lENGTH :: Int
lENGTH = 10000

djb2 :: BS.ByteString -> Int
djb2 bs = BS.foldl' (\hash w -> (hash `shiftL` 5) + hash + (fromIntegral w)) 5381 bs

new :: forall s a. ST s (STArray s Int (Element a))
new = newArray (0,lENGTH-1) Empty

lookupST :: forall s a. BS.ByteString -> (STArray s Int (Element a)) -> ST s (Maybe (Pair a))
lookupST key ht = do
    let idx = djb2 key `mod` lENGTH
    element <- unsafeRead ht idx
    pure $ case element of
        Single presentPair
            | pairKey presentPair == key -> Just presentPair
            | otherwise -> Nothing
        Collision pairs -> case List.findIndex (\p -> key == pairKey p) pairs of
            Just found -> Just $ pairs !! found
            Nothing -> Nothing
        Empty -> Nothing

lookup :: forall a. BS.ByteString -> Array Int (Element a) -> a
lookup key ht = let
    idx = djb2 key `mod` lENGTH
    element = ht ! idx
    in case element of
        Single presentPair
            | pairKey presentPair == key -> pairValue $ presentPair
            | otherwise -> error $ "not found: " ++ show key
        Collision pairs -> case List.findIndex (\p -> key == pairKey p) pairs of
            Just found -> pairValue $ pairs !! found
            Nothing -> error $ "not found: " ++ show key
        Empty -> error $ "not found: " ++ show key

insertWith :: forall s a. (a -> a -> a) -> Pair a -> STArray s Int (Element a) -> ST s Bool
insertWith f kv ht = do
    let idx = djb2 (pairKey kv) `mod` lENGTH
    el <- unsafeRead ht idx
    let !(modified, inserted) = upsert f kv el
    unsafeWrite ht idx modified
    pure inserted

{-# SCC upsert #-}
{-# INLINE upsert #-}
upsert :: (a -> a -> a) -> Pair a -> Element a -> (Element a, Bool)
upsert f pair element = case element of
    Single presentPair
        | pairKey presentPair == pairKey pair -> {-# SCC upsertMergeSingle #-} (Single (presentPair { pairValue =  (f (pairValue presentPair) (pairValue pair)) }), False)
        | otherwise -> {-# SCC upsertCollideSingle #-} (Collision [pair, presentPair], True)
    Collision pairs -> case List.findIndex (\p -> pairKey pair == pairKey p) pairs of
        Just idx -> {-# SCC upsertMergeCollide #-} let
            (pre,(p:post)) = splitAt idx pairs
            p' = p { pairValue = f (pairValue p) (pairValue pair) }
            in (Collision (pre <> (p':post)), False)
        Nothing -> {-# SCC upsertGrowCollide #-} (Collision (pair:pairs), True)
    Empty -> {-# SCC upsertInsertEmpty #-} (Single pair, True)
