{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module OneBRC
    ( processAll
    , formatOutput
    , makeChunks
    ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (foldM)
-- import Control.Monad.ST
-- import GHC.Conc (par)
-- import Data.Array (Array)
-- import Data.Array.MArray (freeze)
-- import Data.Array.ST (STArray)
-- import Data.Char (ord)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Internal (c2w)
import qualified Data.ByteString.Unsafe as BU
-- import qualified Data.Map.Strict as Map
import qualified Data.List as L
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Text.Printf

import Hashtable as HT

data Measure = Measure
    { measureMin :: !Int
    , measureMax :: !Int
    , measureSum :: !Int
    , measureCount :: !Int
    } deriving (Eq, Show)

-- mergeMeasurementMap :: Map Text Measure -> Map Text Measure -> Map Text Measure
-- mergeMeasurementMap = Map.unionWith mergeMeasure

formatMeasure :: Measure -> Text
formatMeasure m = T.pack $ printf "%.1f/%.1f/%.1f" (fromIntegral (measureMin m) / 10 :: Double) (fromIntegral (measureSum m) / (fromIntegral (10 * measureCount m)) :: Double) (fromIntegral (measureMax m) / 10 :: Double)

{-# SCC parseTemp #-}
parseTemp :: BS.ByteString -> Int
parseTemp bs = case BU.unsafeIndex bs 0 of
    -- c2w '-' == 45
    45 -> negate $ parseTemp (BU.unsafeTail bs)
    c0 -> case BU.unsafeIndex bs 1 of
        -- c2w '.' == 46
        46 -> let
            c1 = BU.unsafeIndex bs 2
            in (d2i c0 * 10) + (d2i c1)
        c1 -> let
            c2 = BU.unsafeIndex bs 3
            in (d2i c0 * 100) + (d2i c1 * 10) + (d2i c2)
    -- c2w '0' == 48
    where d2i c = (fromIntegral c) - 48

{-# SCC parseLine #-}
parseLine :: BS.ByteString -> (BS.ByteString, Measure)
parseLine line = case BS.elemIndex 59 line of
    Just n -> let
        name = BU.unsafeTake n line
        m = parseTemp $ BU.unsafeDrop (n+1) line
        in (name, Measure m m m 1)
    Nothing -> error $ "no ; found in " <> show line

{-# SCC mergeMeasure #-}
mergeMeasure :: Measure -> Measure -> Measure
mergeMeasure mA mB = Measure
    { measureMin = min (measureMin mA) (measureMin mB)
    , measureMax = max (measureMax mA) (measureMax mB)
    , measureSum = measureSum mA + measureSum mB
    , measureCount = measureCount mA + measureCount mB
    }

processAll :: BS.ByteString -> IO [(Set.Set BS.ByteString, HT.FrozenHashtable Measure)]
processAll fileContents = do
    let chunks = makeChunks 4 fileContents
    results <- mapConcurrently processChunk chunks
    pure results


processChunk :: BS.ByteString -> IO (Set.Set BS.ByteString, HT.FrozenHashtable Measure)
processChunk fileChunk = do
    let input = map parseLine $ BC.lines fileChunk
    ht <- HT.new
    (htFinal, setFinal) <- foldM step (ht,Set.empty) input
    frozen <- {-# SCC "freezeHashtable" #-} HT.freeze htFinal
    pure (setFinal, frozen)

{-# SCC step #-}
step :: (HT.Hashtable Measure, Set.Set BS.ByteString) -> (BS.ByteString, Measure) -> IO (HT.Hashtable Measure, Set.Set BS.ByteString)
step (ht,set) (key,measure) = do
    newKey <- HT.insertWith mergeMeasure key measure ht
    let !set' = {-# SCC "insertSet" #-} if newKey
                    then Set.union set (Set.singleton key)
                    else set
    pure (ht, set')

{-# SCC formatOutput #-}
formatOutput :: [(Set.Set BS.ByteString, HT.FrozenHashtable Measure)] -> Text
formatOutput pairs = let
    (sets, hts) = unzip pairs
    names = Set.toAscList $ Set.unions sets
    measure name = L.foldl1' mergeMeasure $ map (HT.lookup name) hts
    equals = map (\name -> T.concat [decodeUtf8 name, "=", (formatMeasure $ measure name)]) names
    commas = T.intercalate ", " equals
    in T.concat ["{", commas, "}"]

makeChunks :: Int -> BS.ByteString -> [BS.ByteString]
makeChunks _ input | BS.null input = []
makeChunks 0 input = [input]
makeChunks 1 input = [input]
makeChunks n input = let
    len = BS.length input
    size = len `quot` n
    bigger = BS.take size input
    newline = BS.elemIndexEnd (c2w '\n') bigger
    in case newline of
        Just end -> (BS.take end input) : makeChunks (n-1) (BS.drop (end+1) input)
        Nothing -> [input]
