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

uPPER_BOUND :: Int
uPPER_BOUND = 9999

djb2 :: BS.ByteString -> Int
djb2 bs = BS.foldl' (\hash w -> (hash `shiftL` 5) + hash + (fromIntegral w)) 5381 bs

new :: forall s a. ST s (STArray s Int (Element a))
new = newArray (0,uPPER_BOUND) Empty

lookupST :: forall s a. BS.ByteString -> (STArray s Int (Element a)) -> ST s (Maybe (Pair a))
lookupST key ht = do
    let idx = djb2 key `mod` uPPER_BOUND
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
    idx = djb2 key `mod` uPPER_BOUND
    element = ht ! idx
    in case element of
        Single presentPair
            | pairKey presentPair == key -> pairValue $ presentPair
            | otherwise -> error $ "not found: " ++ show key
        Collision pairs -> case List.findIndex (\p -> key == pairKey p) pairs of
            Just found -> pairValue $ pairs !! found
            Nothing -> error $ "not found: " ++ show key
        Empty -> error $ "not found: " ++ show key

insertWith :: forall s a. (a -> a -> a) -> Pair a -> STArray s Int (Element a) -> ST s ()
insertWith f kv ht = do
    let idx = djb2 (pairKey kv) `mod` uPPER_BOUND
    el <- unsafeRead ht idx
    let !modified = upsert f kv el
    unsafeWrite ht idx modified

{-# SCC upsert #-}
{-# INLINE upsert #-}
upsert :: (a -> a -> a) -> Pair a -> Element a -> Element a
upsert f pair element = case element of
    Single presentPair
        | pairKey presentPair == pairKey pair -> {-# SCC upsertMergeSingle #-} Single $ presentPair { pairValue =  (f (pairValue presentPair) (pairValue pair)) }
        | otherwise -> {-# SCC upsertCollideSingle #-} Collision [pair, presentPair]
    Collision pairs -> case List.findIndex (\p -> pairKey pair == pairKey p) pairs of
        Just idx -> {-# SCC upsertMergeCollide #-} let
            (pre,(p:post)) = splitAt idx pairs
            p' = p { pairValue = f (pairValue p) (pairValue pair) }
            in Collision $ pre <> (p':post)
        Nothing -> {-# SCC upsertGrowCollide #-} Collision $ pair:pairs
    Empty -> {-# SCC upsertInsertEmpty #-} Single pair
