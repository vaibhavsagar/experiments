{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module OneBRC
    ( processAll
    , formatMap
    , formatOutput
    ) where

import Control.Parallel (par)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Map.Merge.Strict
import Data.List (foldl')
import Data.Text (Text)
import Data.Text.Read
import qualified Data.Text as T
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

parseLine :: Text -> (Text, Measure)
parseLine line = let
    (name, rest) = T.break (==';') line
    (_, numberText) = T.breakOnEnd ";" rest
    measurement = signed double numberText
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

insertLine :: Map.Map Text Measure -> Text -> Map.Map Text Measure
insertLine measurements line = let
    (name, measure) = {-# SCC parseLine #-} parseLine line
    measurements' = {-# SCC insertMeasurement #-} Map.insertWith mergeMeasure name measure measurements
    in measurements'

processAll :: Int -> Text -> Map.Map Text Measure
processAll chunkSize fileContents =  foldl' insertLine Map.empty $ T.lines fileContents
    -- foldParallel
    --     chunkSize
    --     (foldl' insertLine Map.empty)
    --     mergeMeasurementMap
    --     (T.lines fileContents)

formatMap :: Map.Map Text Measure -> Map.Map Text Text
formatMap = Map.map formatMeasure

formatOutput :: Map.Map Text Text -> Text
formatOutput formattedMap = let
    pairs = Map.toAscList formattedMap
    equals = map (\(k,v) -> T.concat [k, "=", v]) pairs
    commas = T.intercalate ", " equals
    in T.concat ["{", commas, "}"]
