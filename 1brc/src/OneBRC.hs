{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module OneBRC
    ( processAll
    , formatOutput
    ) where

import Control.Monad (foldM)
import Control.Monad.ST
import GHC.Conc (par)
import Data.Array (Array)
import Data.Array.MArray (freeze)
import Data.Char (digitToInt)
import Data.Maybe (fromJust)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
-- import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Text.Printf

import Hashtable as HT

foldParallel :: Int -> ([a] -> b) -> (b -> b -> b) -> [a] -> b
foldParallel _ fold _ [] = fold []
foldParallel chunkSize fold combine xs = par lf $ combine lf rf
    where
        (left, right) = splitAt chunkSize xs
        lf = fold left
        rf = foldParallel chunkSize fold combine right

mergeMeasurementMap :: Map Text Measure -> Map Text Measure -> Map Text Measure
mergeMeasurementMap = Map.unionWith mergeMeasure

formatMeasure :: Measure -> Text
formatMeasure m = T.pack $ printf "%.1f/%.1f/%.1f" (measureMin m) (measureSum m / (fromIntegral (measureCount m))) (measureMax m)

parseTemp :: BS.ByteString -> Double
parseTemp bs = case BC.index bs 0 of
    '-' -> negate $ parseTemp (BC.tail bs)
    c0 -> case BC.index bs 1 of
        '.' -> let
            c1 = BC.index bs 2
            in (fromIntegral $ d2i c0) + ((fromIntegral $ d2i c1) / 10)
        c1 -> let
            c2 = BC.index bs 3
            in (fromIntegral ((d2i c0 * 10) + d2i c1)) + ((fromIntegral $ d2i c2) / 10)
    where d2i = digitToInt

parseLine :: BS.ByteString -> (BS.ByteString, Measure)
parseLine line = let
    (name, rest) = BS.break (==(fromIntegral $ fromEnum ';')) line
    m = parseTemp $ BS.tail rest
    in (name, Measure m m m 1)

mergeMeasure :: Measure -> Measure -> Measure
mergeMeasure mA mB = Measure
    { measureMin = min (measureMin mA) (measureMin mB)
    , measureMax = max (measureMax mA) (measureMax mB)
    , measureSum = measureSum mA + measureSum mB
    , measureCount = measureCount mA + measureCount mB
    }

processAll :: Int -> BS.ByteString -> Map.Map BS.ByteString Measure
processAll _chunkSize fileContents = Map.fromListWith mergeMeasure $ map parseLine $ BC.lines fileContents
    -- runST $ do
    -- let input = map parseLine $ BC.lines fileContents
    -- ht <- HT.new
    -- (htFinal, setFinal) <- foldM step (ht,Set.empty) input
    -- frozen <- freeze htFinal
    -- pure (setFinal, frozen)
    -- where
    --     step (ht,set) (key,measure) = do
    --         HT.insertWith mergeMeasure (Pair key measure) ht
    --         pure (ht, Set.insert key set)

formatOutput :: Map.Map BS.ByteString Measure -> Text
formatOutput measurements = let
    pairs = Map.toAscList measurements
    equals = map (\(k,v) -> T.concat [decodeUtf8 k, "=", formatMeasure v]) pairs
    commas = T.intercalate ", " equals
    in T.concat ["{", commas, "}"]
