{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module OneBRC
    ( processAll
    , formatOutput
    ) where

import Control.Monad (foldM)
import Control.Monad.ST
-- import Data.ByteString.Internal (c2w)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Unsafe as BU
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

formatMeasure :: Measure -> Text
formatMeasure m = T.pack $ printf "%.1f/%.1f/%.1f" (fromIntegral (measureMin m) / 10 :: Double) (fromIntegral (measureSum m) / (fromIntegral (10 * measureCount m)) :: Double) (fromIntegral (measureMax m) / 10 :: Double)

{-# SCC parseLine #-}
parseLine :: BS.ByteString -> (BS.ByteString, Measure)
parseLine input = let
    len = BS.length input
    ones = d2i $ BU.unsafeIndex input (len-1)
    tens = 10 * (d2i $ BU.unsafeIndex input (len-3))
    in case BU.unsafeIndex input (len-4) of
        -- c2w ';' == 59
        59 -> let
            -- ";d.d"
            name = BU.unsafeTake (len-4) input
            m = tens + ones
            in (name, Measure m m m 1)
        -- c2w '-' == 45
        45 -> let
            -- ";-d.d"
            name = BU.unsafeTake (len-5) input
            m = negate $ tens + ones
            in (name, Measure m m m 1)
        hundreds -> case BU.unsafeIndex input (len-5) of
            -- c2w ';' == 59
            59 -> let
                -- ";dd.d"
                name = BU.unsafeTake (len-5) input
                m = (100*(d2i hundreds)) + tens + ones
                in (name, Measure m m m 1)
            -- c2w '-' == 45
            45 -> let
                -- ";-dd.d"
                name = BU.unsafeTake (len-6) input
                m = negate $ (100*(d2i hundreds)) + tens + ones
                in (name, Measure m m m 1)
            _ -> error $ "incorrectly formatted: " <> show input
    where d2i c = (fromIntegral c) - 48

{-# SCC mergeMeasure #-}
mergeMeasure :: Measure -> Measure -> Measure
mergeMeasure mA mB = Measure
    { measureMin = min (measureMin mA) (measureMin mB)
    , measureMax = max (measureMax mA) (measureMax mB)
    , measureSum = measureSum mA + measureSum mB
    , measureCount = measureCount mA + measureCount mB
    }

processAll :: BS.ByteString -> (Set.Set BS.ByteString, HT.FrozenHashtable Measure)
processAll fileContents = runST $ do
    ht <- HT.new
    (htFinal, setFinal) <- foldM step (ht,Set.empty) $ map parseLine $ BC.lines fileContents
    frozen <- {-# SCC "freezeHashtable" #-} HT.freeze htFinal
    pure (setFinal, frozen)

{-# SCC step #-}
step :: forall s. (HT.Hashtable s Measure, Set.Set BS.ByteString) -> (BS.ByteString, Measure) -> ST s (HT.Hashtable s Measure, Set.Set BS.ByteString)
step (ht,set) (key,measure) = do
    newKey <- HT.insertWith mergeMeasure key measure ht
    let !set' = {-# SCC "insertSet" #-} if newKey
                    then Set.union set (Set.singleton key)
                    else set
    pure (ht, set')

{-# SCC formatOutput #-}
formatOutput :: (Set.Set BS.ByteString, HT.FrozenHashtable Measure) -> Text
formatOutput (set, ht) = let
    names = Set.toAscList set
    equals = map (\name -> T.concat [decodeUtf8 name, "=", (formatMeasure $ HT.lookup name ht)]) names
    commas = T.intercalate ", " equals
    in T.concat ["{", commas, "}"]
