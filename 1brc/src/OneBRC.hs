{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module OneBRC
    ( processAll
    , formatOutput
    ) where

import GHC.Conc (par)
import Data.Map.Strict (Map)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as Map
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Read
import Text.Printf

data Measure = Measure
    { measureMin :: !Double
    , measureMax :: !Double
    , measureSum :: !Double
    , measureCount :: !Int
    } deriving (Eq, Show)

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

parseLine :: BS.ByteString -> (BS.ByteString, Measure)
parseLine line = let
    [name, rest] = BS.split (fromIntegral $ fromEnum ';') line
    measurement = signed double $ decodeUtf8 rest
    in case measurement of
        Left err -> error err
        Right (m, _) -> (name, Measure m m m 1)

mergeMeasure :: Measure -> Measure -> Measure
mergeMeasure mA mB = Measure
    { measureMin = min (measureMin mA) (measureMin mB)
    , measureMax = max (measureMax mA) (measureMax mB)
    , measureSum = measureSum mA + measureSum mB
    , measureCount = measureCount mA + measureCount mB
    }

insertLine :: Map.Map BS.ByteString Measure -> BS.ByteString -> Map.Map BS.ByteString Measure
insertLine measurements line = let
    (name, measure) = {-# SCC parseLine #-} parseLine line
    measurements' = {-# SCC insertMeasurement #-} Map.insertWith mergeMeasure name measure measurements
    in measurements'

processAll :: Int -> BS.ByteString -> Map.Map BS.ByteString Measure
processAll _chunkSize fileContents =  foldl' insertLine Map.empty $ BC.lines fileContents
    -- foldParallel
    --     chunkSize
    --     (foldl' insertLine Map.empty)
    --     mergeMeasurementMap
    --     (T.lines fileContents)

formatOutput :: Map.Map BS.ByteString Measure -> Text
formatOutput formattedMap = let
    pairs = Map.toAscList formattedMap
    equals = map (\(k,v) -> T.concat [decodeUtf8 k, "=", formatMeasure v]) pairs
    commas = T.intercalate ", " equals
    in T.concat ["{", commas, "}"]
