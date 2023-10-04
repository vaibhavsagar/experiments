{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module Harimau (someFunc) where

import Foreign
import Foreign.C.Types
import Foreign.Marshal.Array
import Numeric.Natural

data RawRoaringBitmap

newtype RoaringBitmap = RoaringBitmap (ForeignPtr RawRoaringBitmap) deriving (Eq)

foreign import ccall safe "roaring.h roaring_bitmap_create"
    c_roaring_bitmap_create :: IO (Ptr RawRoaringBitmap)

foreign import ccall safe "roaring.h &roaring_bitmap_free"
    c_roaring_bitmap_free :: FinalizerPtr RawRoaringBitmap

foreign import ccall safe "roaring.h roaring_bitmap_printf"
    c_roaring_bitmap_printf :: Ptr RawRoaringBitmap -> IO ()

foreign import ccall safe "roaring.h roaring_bitmap_from_range"
    c_roaring_bitmap_from_range :: CULong -> CULong -> CUInt -> IO (Ptr RawRoaringBitmap)

foreign import ccall safe "roaring.h roaring_bitmap_of_ptr"
    c_roaring_bitmap_of_ptr :: CSize -> Ptr CUInt -> IO (Ptr RawRoaringBitmap)

foreign import ccall safe "roaring.h roaring_bitmap_copy"
    c_roaring_bitmap_copy :: Ptr RawRoaringBitmap -> IO (Ptr RawRoaringBitmap)

foreign import ccall safe "roaring.h roaring_bitmap_overwrite"
    c_roaring_bitmap_overwrite :: Ptr RawRoaringBitmap -> Ptr RawRoaringBitmap -> IO CBool

foreign import ccall safe "roaring.h roaring_bitmap_and"
    c_roaring_bitmap_and :: Ptr RawRoaringBitmap -> Ptr RawRoaringBitmap -> IO (Ptr RawRoaringBitmap)

foreign import ccall safe "roaring.h roaring_bitmap_and_cardinality"
    c_roaring_bitmap_and_cardinality :: Ptr RawRoaringBitmap -> Ptr RawRoaringBitmap -> IO CULong

foreign import ccall safe "roaring.h roaring_bitmap_intersect"
    c_roaring_bitmap_intersect :: Ptr RawRoaringBitmap -> Ptr RawRoaringBitmap -> IO CBool

roaringBitmapFromRawPtr :: Ptr RawRoaringBitmap -> IO RoaringBitmap
roaringBitmapFromRawPtr ptr = RoaringBitmap <$> newForeignPtr c_roaring_bitmap_free ptr

createRoaringBitmap :: IO RoaringBitmap
createRoaringBitmap = roaringBitmapFromRawPtr =<< c_roaring_bitmap_create

roaringBitmapPrintf :: RoaringBitmap -> IO ()
roaringBitmapPrintf (RoaringBitmap rbm) = withForeignPtr rbm c_roaring_bitmap_printf

roaringBitmapFromRange :: Natural -> Natural -> Natural -> IO RoaringBitmap
roaringBitmapFromRange min max step = roaringBitmapFromRawPtr =<< c_roaring_bitmap_from_range (fromIntegral min) (fromIntegral max) (fromIntegral step)

roaringBitmapOfPtr :: [Int] -> IO RoaringBitmap
roaringBitmapOfPtr vals = do
    let ints = map fromIntegral vals
    roaringBitmapFromRawPtr =<< withArrayLen ints (c_roaring_bitmap_of_ptr . fromIntegral)

roaringBitmapCopy :: RoaringBitmap -> IO RoaringBitmap
roaringBitmapCopy (RoaringBitmap rbm) = withForeignPtr rbm $ \ptr ->
    roaringBitmapFromRawPtr =<< c_roaring_bitmap_copy ptr

roaringBitmapAnd :: RoaringBitmap -> RoaringBitmap -> IO RoaringBitmap
roaringBitmapAnd (RoaringBitmap r1) (RoaringBitmap r2) = do
    withForeignPtr r1 $ \ptr1 ->
        withForeignPtr r2 $ \ptr2 ->
            roaringBitmapFromRawPtr =<< c_roaring_bitmap_and ptr1 ptr2

someFunc :: IO ()
someFunc = do
    rbmFromRange <- roaringBitmapFromRange 0 100 10
    roaringBitmapPrintf rbmFromRange
    rbmOfPtr <- roaringBitmapOfPtr [0..20]
    roaringBitmapPrintf rbmOfPtr
    intersect <- roaringBitmapAnd rbmFromRange rbmOfPtr
    roaringBitmapPrintf intersect
    pure ()
