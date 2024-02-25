module OneBRC where

import qualified Data.Map.Strict as Map
import Data.List
import Text.Printf

data Measure = Measure
    { measureMin :: Double
    , measureMax :: Double
    , measureAvg :: Double
    } deriving (Eq, Show)

formatMeasure :: Measure -> String
formatMeasure m = printf "%.1f/%.1f/%.1f" (measureMin m) (measureAvg m) (measureMax m)

parseLine :: String -> (String, Double)
parseLine line = let
    (name, rest) = break (==';') line
    measurement = read (tail rest)
    in (name, measurement)

insertLine :: Map.Map String [Double] -> String -> Map.Map String [Double]
insertLine measurements line = let
    (name, measurement) = parseLine line
    measurements' = Map.insertWith (<>) name [measurement] measurements
    in measurements'

calculateMeasure :: [Double] -> Measure
calculateMeasure ds = Measure
    { measureMin = minimum ds
    , measureMax = maximum ds
    , measureAvg = sum ds / genericLength ds
    }

processAll :: String -> Map.Map String [Double]
processAll fileContents = foldl' insertLine Map.empty (lines fileContents)

calculateAll :: Map.Map String [Double] -> Map.Map String Measure
calculateAll = Map.map calculateMeasure

formatMap :: Map.Map String Measure -> Map.Map String String
formatMap = Map.map formatMeasure

formatOutput :: Map.Map String String -> String
formatOutput formattedMap = let
    pairs = Map.toAscList formattedMap
    equals = map (\(k,v) -> k ++ "=" ++ v) pairs
    commas = intercalate ", " equals
    in "{" ++ commas ++ "}"
