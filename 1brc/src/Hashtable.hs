{-# Language RankNTypes #-}
{-# Language BangPatterns #-}
{-# Language ScopedTypeVariables #-}

module Hashtable where

import Control.Monad.ST
import Data.Array
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
    = Empty
    | Single (Pair a)
    | Collision [Pair a]
    deriving (Eq, Show)

djb2 :: BS.ByteString -> Int
djb2 bs = BS.foldl' (\hash w -> (hash `shiftL` 5) + hash + (fromIntegral w)) 5381 bs

new :: forall s a. ST s (STArray s Int (Element a))
new = newArray (0,20000) Empty

lookupST :: forall s a. BS.ByteString -> (STArray s Int (Element a)) -> ST s (Maybe (Pair a))
lookupST key ht = do
    upperBound <- snd <$> getBounds ht
    let idx = djb2 key `mod` upperBound
    element <- readArray ht idx
    pure $ case element of
        Empty -> Nothing
        Single presentPair
            | pairKey presentPair == key -> Just presentPair
            | otherwise -> Nothing
        Collision pairs -> case List.findIndex (\p -> key == pairKey p) pairs of
            Just found -> Just $ pairs !! found
            Nothing -> Nothing

lookup :: forall a. BS.ByteString -> Array Int (Element a) -> Maybe (Pair a)
lookup key ht = let
    upperBound = snd $ bounds ht
    idx = djb2 key `mod` upperBound
    element = ht ! idx
    in case element of
        Empty -> Nothing
        Single presentPair
            | pairKey presentPair == key -> Just presentPair
            | otherwise -> Nothing
        Collision pairs -> case List.findIndex (\p -> key == pairKey p) pairs of
            Just found -> Just $ pairs !! found
            Nothing -> Nothing

insertWith :: forall s a. (a -> a -> a) -> Pair a -> STArray s Int (Element a) -> ST s ()
insertWith f kv ht = do
    upperBound <- snd <$> getBounds ht
    let idx = djb2 (pairKey kv) `mod` upperBound
    modifyArray' ht idx (upsert f kv)

upsert :: (a -> a -> a) -> Pair a -> Element a -> Element a
upsert f pair element = case element of
    Empty -> Single pair
    Single presentPair
        | pairKey presentPair == pairKey pair -> Single $ Pair (pairKey pair) (f (pairValue presentPair) (pairValue pair))
        | otherwise -> Collision [pair, presentPair]
    Collision pairs -> case List.findIndex (\p -> pairKey pair == pairKey p) pairs of
        Just idx -> let
            (pre,(p:post)) = splitAt idx pairs
            p' = p { pairValue = f (pairValue p) (pairValue pair) }
            in Collision $ pre <> (p':post)
        Nothing -> Collision $ pair:pairs
