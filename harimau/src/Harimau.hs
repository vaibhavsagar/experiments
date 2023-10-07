module Harimau where

import Control.Monad.Trans.Cont (ContT (..))
import Control.Monad.IO.Class (liftIO)
import Data.Traversable (for)
import Foreign
import Foreign.C.Types
import Foreign.Marshal.Array
import Numeric.Natural

import Harimau.Bindings

newtype RoaringBitmap = RoaringBitmap { unRoaringBitmap :: ForeignPtr RawRoaringBitmap } deriving (Eq)

roaringBitmapFromRawPtr :: Ptr RawRoaringBitmap -> IO RoaringBitmap
roaringBitmapFromRawPtr ptr = RoaringBitmap <$> newForeignPtr c_roaring_bitmap_free ptr

createRoaringBitmap :: IO RoaringBitmap
createRoaringBitmap = roaringBitmapFromRawPtr =<< c_roaring_bitmap_create

roaringBitmapPrintf :: RoaringBitmap -> IO ()
roaringBitmapPrintf rbm = withForeignPtr (unRoaringBitmap rbm) c_roaring_bitmap_printf

roaringBitmapFromRange :: Natural -> Natural -> Natural -> IO RoaringBitmap
roaringBitmapFromRange min max step = roaringBitmapFromRawPtr =<< c_roaring_bitmap_from_range (fromIntegral min) (fromIntegral max) (fromIntegral step)

roaringBitmapOfPtr :: [Int] -> IO RoaringBitmap
roaringBitmapOfPtr vals = do
    let ints = map fromIntegral vals
    roaringBitmapFromRawPtr =<< withArrayLen ints (c_roaring_bitmap_of_ptr . fromIntegral)

roaringBitmapCopy :: RoaringBitmap -> IO RoaringBitmap
roaringBitmapCopy rbm = withForeignPtr (unRoaringBitmap rbm) $ \ptr ->
    roaringBitmapFromRawPtr =<< c_roaring_bitmap_copy ptr

roaringBitmapAnd :: RoaringBitmap -> RoaringBitmap -> IO RoaringBitmap
roaringBitmapAnd r1 r2 = do
    withForeignPtr (unRoaringBitmap r1) $ \ptr1 ->
        withForeignPtr (unRoaringBitmap r2) $ \ptr2 ->
            roaringBitmapFromRawPtr =<< c_roaring_bitmap_and ptr1 ptr2

roaringBitmapOr :: RoaringBitmap -> RoaringBitmap -> IO RoaringBitmap
roaringBitmapOr r1 r2 = do
    withForeignPtr (unRoaringBitmap r1) $ \ptr1 ->
        withForeignPtr (unRoaringBitmap r2) $ \ptr2 ->
            roaringBitmapFromRawPtr =<< c_roaring_bitmap_or ptr1 ptr2

-- https://ro-che.info/articles/2019-06-07-why-use-contt#reading-from-a-list-of-files
roaringBitmapOrMany :: [RoaringBitmap] -> IO RoaringBitmap
roaringBitmapOrMany bitmaps = flip runContT pure $ do
    ptrs <- for bitmaps $ \roaringBitmap ->
        ContT . withForeignPtr $ unRoaringBitmap roaringBitmap
    liftIO $ roaringBitmapFromRawPtr =<< withArrayLen ptrs (c_roaring_bitmap_or_many . fromIntegral)

-- https://ro-che.info/articles/2019-06-07-why-use-contt#reading-from-a-list-of-files
roaringBitmapOrManyHeap :: [RoaringBitmap] -> IO RoaringBitmap
roaringBitmapOrManyHeap bitmaps = flip runContT pure $ do
    ptrs <- for bitmaps $ \roaringBitmap ->
        ContT . withForeignPtr $ unRoaringBitmap roaringBitmap
    liftIO $ roaringBitmapFromRawPtr =<< withArrayLen ptrs (c_roaring_bitmap_or_many_heap . fromIntegral)

someFunc :: IO ()
someFunc = do
    rbmFromRange <- roaringBitmapFromRange 0 100 10
    roaringBitmapPrintf rbmFromRange
    rbmOfPtr <- roaringBitmapOfPtr [0..20]
    roaringBitmapPrintf rbmOfPtr
    intersect <- roaringBitmapAnd rbmFromRange rbmOfPtr
    roaringBitmapPrintf intersect
    union <- roaringBitmapOrManyHeap [rbmFromRange, rbmOfPtr, intersect]
    roaringBitmapPrintf union
    pure ()
