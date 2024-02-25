{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module OneBRC where

import qualified Data.Map.Strict as Map
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

formatMeasure :: Measure -> Text
formatMeasure m = T.pack $ printf "%.1f/%.1f/%.1f" (measureMin m) (measureSum m / (fromIntegral (measureCount m))) (measureMax m)

parseLine :: Text -> (Text, Double)
parseLine line = let
    (name, rest) = T.break (==';') line
    (_, numberText) = T.breakOnEnd ";" rest
    measurement = signed double numberText
    in case measurement of
        Left err -> error err
        Right (parsed, _) -> (name, parsed)

mergeMeasure :: Measure -> Measure -> Measure
mergeMeasure mA mB = Measure
    { measureMin = min (measureMin mA) (measureMin mB)
    , measureMax = max (measureMax mA) (measureMax mB)
    , measureSum = measureSum mA + measureSum mB
    , measureCount = measureCount mA + measureCount mB
    }

insertLine :: Map.Map Text Measure -> Text -> Map.Map Text Measure
insertLine measurements line = let
    (name, measurement) = parseLine line
    measure = Measure measurement measurement measurement 1
    measurements' = Map.insertWith mergeMeasure name measure measurements
    in measurements'

processAll :: Text -> Map.Map Text Measure
processAll fileContents = foldl' insertLine Map.empty (T.lines fileContents)

formatMap :: Map.Map Text Measure -> Map.Map Text Text
formatMap = Map.map formatMeasure

formatOutput :: Map.Map Text Text -> Text
formatOutput formattedMap = let
    pairs = Map.toAscList formattedMap
    equals = map (\(k,v) -> T.concat [k, "=", v]) pairs
    commas = T.intercalate ", " equals
    in T.concat ["{", commas, "}"]
