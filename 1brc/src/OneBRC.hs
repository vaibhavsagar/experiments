{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module OneBRC
    ( processAll
    , formatOutput
    ) where

import Control.Monad (foldM)
import Control.Monad.ST
-- import GHC.Conc (par)
import Data.Array (Array)
import Data.Array.MArray (freeze)
import Data.Array.ST (STArray)
-- import Data.Char (ord)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
-- import Data.ByteString.Internal (c2w, w2c)
import qualified Data.ByteString.Unsafe as BU
-- import qualified Data.Map.Strict as Map
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

processAll :: BS.ByteString -> (Set.Set BS.ByteString, Array Int (Element Measure))
processAll fileContents = runST $ do
    let input = map parseLine $ BC.lines fileContents
    ht <- HT.new
    (htFinal, setFinal) <- foldM step (ht,Set.empty) input
    frozen <- {-# SCC "freezeHashtable" #-} freeze htFinal
    pure (setFinal, frozen)

{-# SCC step #-}
step :: forall s. (STArray s Int (Element Measure), Set.Set BS.ByteString) -> (BS.ByteString, Measure) -> ST s (STArray s Int (Element Measure), Set.Set BS.ByteString)
step (ht,set) (key,measure) = do
    newKey <- HT.insertWith mergeMeasure (Pair key measure) ht
    let !set' = {-# SCC "insertSet" #-} if newKey
                    then Set.union set (Set.singleton key)
                    else set
    pure (ht, set')

{-# SCC formatOutput #-}
formatOutput :: (Set.Set BS.ByteString, Array Int (Element Measure)) -> Text
formatOutput (set, ht) = let
    names = Set.toAscList set
    equals = map (\name -> T.concat [decodeUtf8 name, "=", (formatMeasure $ HT.lookup name ht)]) names
    commas = T.intercalate ", " equals
    in T.concat ["{", commas, "}"]
