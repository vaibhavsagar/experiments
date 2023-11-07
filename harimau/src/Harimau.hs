{-# LANGUAGE ScopedTypeVariables #-}

module Harimau where

import Control.Monad.Trans.Cont (ContT (..))
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString, packCStringLen, useAsCString, useAsCStringLen)
import Data.Bits ()
import Data.Coerce (coerce)
import Data.Traversable (for)
import Foreign
import Foreign.C.Types
import Foreign.Marshal.Array
import Numeric.Natural
import qualified System.IO
import System.IO.Unsafe (unsafePerformIO)

import Harimau.Bindings

newtype RoaringBitmap = RoaringBitmap { unRoaringBitmap :: ForeignPtr RawRoaringBitmap }

instance Eq RoaringBitmap where
    a == b = unsafePerformIO $ roaringBitmapEquals a b

instance Bits RoaringBitmap where
    a .&. b = unsafePerformIO $ roaringBitmapAnd a b
    a .|. b = unsafePerformIO $ roaringBitmapOr a b
    xor a b = unsafePerformIO $ roaringBitmapXor a b
    complement a = unsafePerformIO $ roaringBitmapXor a =<< roaringBitmapFromRange 0 (2^32) 1
    shift a i = unsafePerformIO $ roaringBitmapAddOffset a (fromIntegral i)
    rotate a i = error "undefined"
    bitSize a = undefined
    bitSizeMaybe a = Nothing
    isSigned a = False
    testBit a i = unsafePerformIO $ roaringBitmapContains a (fromIntegral i)
    bit i = unsafePerformIO $ roaringBitmapOfPtr [(fromIntegral i)]
    popCount a = fromIntegral $ unsafePerformIO $ roaringBitmapGetCardinality a

roaringBitmapFromRawPtr :: Ptr RawRoaringBitmap -> IO RoaringBitmap
roaringBitmapFromRawPtr ptr = RoaringBitmap <$> newForeignPtr c_roaring_bitmap_free ptr

with2RoaringBitmaps :: (Ptr RawRoaringBitmap -> Ptr RawRoaringBitmap -> IO a) -> RoaringBitmap -> RoaringBitmap -> IO a
with2RoaringBitmaps fn r1 r2 =
    withForeignPtr (unRoaringBitmap r1) $ \ptr1 ->
        withForeignPtr (unRoaringBitmap r2) $ \ptr2 ->
            fn ptr1 ptr2

createRoaringBitmap :: IO RoaringBitmap
createRoaringBitmap = roaringBitmapFromRawPtr =<< c_roaring_bitmap_create

roaringBitmapPrintf :: RoaringBitmap -> IO ()
roaringBitmapPrintf rbm = withForeignPtr (unRoaringBitmap rbm) c_roaring_bitmap_printf

roaringBitmapFromRange :: Word64 -> Word64 -> Word32 -> IO RoaringBitmap
roaringBitmapFromRange min max step = roaringBitmapFromRawPtr =<< c_roaring_bitmap_from_range (CULong min) (CULong max) (CUInt step)

roaringBitmapOfPtr :: [Word32] -> IO RoaringBitmap
roaringBitmapOfPtr vals =
    roaringBitmapFromRawPtr =<< withArrayLen (map CUInt vals) (c_roaring_bitmap_of_ptr . fromIntegral)

roaringBitmapAddOffset :: RoaringBitmap -> Int64 -> IO RoaringBitmap
roaringBitmapAddOffset rbm offset =
    withForeignPtr (unRoaringBitmap rbm) $ \ptr ->
        roaringBitmapFromRawPtr =<< c_roaring_bitmap_add_offset ptr (CLong offset)

roaringBitmapCopy :: RoaringBitmap -> IO RoaringBitmap
roaringBitmapCopy rbm =
    withForeignPtr (unRoaringBitmap rbm) $ \ptr ->
        roaringBitmapFromRawPtr =<< c_roaring_bitmap_copy ptr

roaringBitmapAnd :: RoaringBitmap -> RoaringBitmap -> IO RoaringBitmap
roaringBitmapAnd =
    with2RoaringBitmaps $
        \ptr1 ptr2 -> roaringBitmapFromRawPtr =<< c_roaring_bitmap_and ptr1 ptr2

roaringBitmapAndCardinality :: RoaringBitmap -> RoaringBitmap -> IO Word64
roaringBitmapAndCardinality = with2RoaringBitmaps $
    \ptr1 ptr2 -> coerce <$> c_roaring_bitmap_and_cardinality ptr1 ptr2

roaringBitmapIntersect :: RoaringBitmap -> RoaringBitmap -> IO Bool
roaringBitmapIntersect = with2RoaringBitmaps $
    \ptr1 ptr2 -> toBool <$> c_roaring_bitmap_intersect ptr1 ptr2

roaringBitmapIntersectWithRange :: RoaringBitmap -> Word64 -> Word64 -> IO Bool
roaringBitmapIntersectWithRange rbm i j =
    withForeignPtr (unRoaringBitmap rbm) $ \ptr ->
        toBool <$> c_roaring_bitmap_intersect_with_range ptr (fromIntegral i) (fromIntegral j)

roaringBitmapJaccardIndex :: RoaringBitmap -> RoaringBitmap -> IO Double
roaringBitmapJaccardIndex = with2RoaringBitmaps $
    \ptr1 ptr2 -> realToFrac <$> c_roaring_bitmap_jaccard_index ptr1 ptr2

roaringBitmapOrCardinality :: RoaringBitmap -> RoaringBitmap -> IO Word64
roaringBitmapOrCardinality = with2RoaringBitmaps $
    \ptr1 ptr2 -> coerce <$> c_roaring_bitmap_or_cardinality ptr1 ptr2

roaringBitmapAndNotCardinality :: RoaringBitmap -> RoaringBitmap -> IO Word64
roaringBitmapAndNotCardinality = with2RoaringBitmaps $
    \ptr1 ptr2 -> coerce <$> c_roaring_bitmap_andnot_cardinality ptr1 ptr2

roaringBitmapXorCardinality :: RoaringBitmap -> RoaringBitmap -> IO Word64
roaringBitmapXorCardinality = with2RoaringBitmaps $
    \ptr1 ptr2 -> coerce <$> c_roaring_bitmap_xor_cardinality ptr1 ptr2

roaringBitmapAndInplace :: RoaringBitmap -> RoaringBitmap -> IO ()
roaringBitmapAndInplace = with2RoaringBitmaps c_roaring_bitmap_and_inplace

roaringBitmapOr :: RoaringBitmap -> RoaringBitmap -> IO RoaringBitmap
roaringBitmapOr = with2RoaringBitmaps $
    \ptr1 ptr2 -> roaringBitmapFromRawPtr =<< c_roaring_bitmap_or ptr1 ptr2

roaringBitmapOrInplace :: RoaringBitmap -> RoaringBitmap -> IO ()
roaringBitmapOrInplace = with2RoaringBitmaps c_roaring_bitmap_or_inplace

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

roaringBitmapXor :: RoaringBitmap -> RoaringBitmap -> IO RoaringBitmap
roaringBitmapXor = with2RoaringBitmaps $
    \ptr1 ptr2 -> roaringBitmapFromRawPtr =<< c_roaring_bitmap_xor ptr1 ptr2

roaringBitmapXorInplace :: RoaringBitmap -> RoaringBitmap -> IO ()
roaringBitmapXorInplace = with2RoaringBitmaps c_roaring_bitmap_xor_inplace

roaringBitmapXorMany :: [RoaringBitmap] -> IO RoaringBitmap
roaringBitmapXorMany bitmaps = flip runContT pure $ do
    ptrs <- for bitmaps $ \roaringBitmap ->
        ContT . withForeignPtr $ unRoaringBitmap roaringBitmap
    liftIO $ roaringBitmapFromRawPtr =<< withArrayLen ptrs (c_roaring_bitmap_xor_many . fromIntegral)

roaringBitmapAndnot :: RoaringBitmap -> RoaringBitmap -> IO RoaringBitmap
roaringBitmapAndnot = with2RoaringBitmaps $
    \ptr1 ptr2 -> roaringBitmapFromRawPtr =<< c_roaring_bitmap_andnot ptr1 ptr2

roaringBitmapAndnotInplace :: RoaringBitmap -> RoaringBitmap -> IO ()
roaringBitmapAndnotInplace = with2RoaringBitmaps c_roaring_bitmap_andnot_inplace

roaringBitmapAddMany :: RoaringBitmap -> [Word32] -> IO ()
roaringBitmapAddMany rbm words =
    withForeignPtr (unRoaringBitmap rbm) $ \ptr ->
        withArrayLen (map CUInt words) (c_roaring_bitmap_add_many ptr . fromIntegral)

roaringBitmapAdd :: RoaringBitmap -> Word32 -> IO ()
roaringBitmapAdd rbm word =
    withForeignPtr (unRoaringBitmap rbm) $ \ptr ->
        c_roaring_bitmap_add ptr (CUInt word)

roaringBitmapAddChecked :: RoaringBitmap -> Word32 -> IO Bool
roaringBitmapAddChecked rbm word =
    withForeignPtr (unRoaringBitmap rbm) $ \ptr ->
        toBool <$> c_roaring_bitmap_add_checked ptr (CUInt word)

roaringBitmapAddRangeClosed :: RoaringBitmap -> Word32 -> Word32 -> IO ()
roaringBitmapAddRangeClosed rbm wordFrom wordTo =
    withForeignPtr (unRoaringBitmap rbm) $ \ptr ->
        c_roaring_bitmap_add_range_closed ptr (CUInt wordFrom) (CUInt wordTo)

roaringBitmapAddRange :: RoaringBitmap -> Word64 -> Word64 -> IO ()
roaringBitmapAddRange rbm wordFrom wordTo =
    withForeignPtr (unRoaringBitmap rbm) $ \ptr ->
        c_roaring_bitmap_add_range ptr (CULong wordFrom) (CULong wordTo)

roaringBitmapRemove :: RoaringBitmap -> Word32 -> IO ()
roaringBitmapRemove rbm word =
    withForeignPtr (unRoaringBitmap rbm) $ \ptr ->
        c_roaring_bitmap_remove ptr (CUInt word)

roaringBitmapRemoveRangeClosed :: RoaringBitmap -> Word32 -> Word32 -> IO ()
roaringBitmapRemoveRangeClosed rbm wordFrom wordTo =
    withForeignPtr (unRoaringBitmap rbm) $ \ptr ->
        c_roaring_bitmap_remove_range_closed ptr (CUInt wordFrom) (CUInt wordTo)

roaringBitmapRemoveRange :: RoaringBitmap -> Word64 -> Word64 -> IO ()
roaringBitmapRemoveRange rbm wordFrom wordTo =
    withForeignPtr (unRoaringBitmap rbm) $ \ptr ->
        c_roaring_bitmap_remove_range ptr (CULong wordFrom) (CULong wordTo)

roaringBitmapRemoveMany :: RoaringBitmap -> [Word32] -> IO ()
roaringBitmapRemoveMany rbm words =
    withForeignPtr (unRoaringBitmap rbm) $ \ptr ->
        withArrayLen (map CUInt words) (c_roaring_bitmap_remove_many ptr . fromIntegral)

roaringBitmapRemoveChecked :: RoaringBitmap -> Word32 -> IO Bool
roaringBitmapRemoveChecked rbm word =
    withForeignPtr (unRoaringBitmap rbm) $ \ptr ->
        toBool <$> c_roaring_bitmap_remove_checked ptr (CUInt word)

roaringBitmapContains :: RoaringBitmap -> Word32 -> IO Bool
roaringBitmapContains rbm word =
    withForeignPtr (unRoaringBitmap rbm) $ \ptr ->
        toBool <$> c_roaring_bitmap_contains ptr (CUInt word)

roaringBitmapContainsRange :: RoaringBitmap -> Word64 -> Word64 -> IO Bool
roaringBitmapContainsRange rbm wordFrom wordTo =
    withForeignPtr (unRoaringBitmap rbm) $ \ptr ->
        toBool <$> c_roaring_bitmap_contains_range ptr (CULong wordFrom) (CULong wordTo)

roaringBitmapGetCardinality :: RoaringBitmap -> IO Word64
roaringBitmapGetCardinality rbm =
    withForeignPtr (unRoaringBitmap rbm) $ \ptr ->
        coerce <$> c_roaring_bitmap_get_cardinality ptr

roaringBitmapIsEmpty :: RoaringBitmap -> IO Bool
roaringBitmapIsEmpty rbm =
    withForeignPtr (unRoaringBitmap rbm) $ \ptr ->
        toBool <$> c_roaring_bitmap_is_empty ptr

roaringBitmapClear :: RoaringBitmap -> IO ()
roaringBitmapClear rbm =
    withForeignPtr (unRoaringBitmap rbm) $ \ptr ->
        c_roaring_bitmap_clear ptr

roaringBitmapToUint32Array :: RoaringBitmap -> IO [Word32]
roaringBitmapToUint32Array rbm =
    withForeignPtr (unRoaringBitmap rbm) $ \ptr -> do
        size <- fromIntegral <$> c_roaring_bitmap_get_cardinality ptr
        allocaArray size $ \ans -> do
            c_roaring_bitmap_to_uint32_array ptr ans
            map coerce <$> peekArray size ans

roaringBitmapRangeUint32Array :: RoaringBitmap -> Word64 -> Word64 -> IO [Word32]
roaringBitmapRangeUint32Array rbm offset limit =
    withForeignPtr (unRoaringBitmap rbm) $ \ptr -> do
        allocaArray (fromIntegral limit) $ \ans -> do
            c_roaring_bitmap_range_uint32_array ptr (CSize offset) (CSize limit) ans
            map coerce <$> peekArray (fromIntegral limit) ans

roaringBitmapRemoveRunCompression :: RoaringBitmap -> IO Bool
roaringBitmapRemoveRunCompression rbm =
    withForeignPtr (unRoaringBitmap rbm) $ \ptr ->
        toBool <$> c_roaring_bitmap_remove_run_compression ptr

roaringBitmapRunOptimize :: RoaringBitmap -> IO Bool
roaringBitmapRunOptimize rbm =
    withForeignPtr (unRoaringBitmap rbm) $ \ptr ->
        toBool <$> c_roaring_bitmap_run_optimize ptr

roaringBitmapShrinkToFit :: RoaringBitmap -> IO Word64
roaringBitmapShrinkToFit rbm =
    withForeignPtr (unRoaringBitmap rbm) $ \ptr ->
        coerce <$> c_roaring_bitmap_shrink_to_fit ptr

roaringBitmapPortableDeserialize :: ByteString -> IO RoaringBitmap
roaringBitmapPortableDeserialize bs =
    roaringBitmapFromRawPtr =<< useAsCString bs c_roaring_bitmap_portable_deserialize

roaringBitmapPortableDeserializeSafe :: ByteString -> IO RoaringBitmap
roaringBitmapPortableDeserializeSafe bs =
    roaringBitmapFromRawPtr =<< useAsCStringLen bs
        (\(ptr, size) -> c_roaring_bitmap_portable_deserialize_safe ptr (fromIntegral size))

roaringBitmapPortableSizeInBytes :: RoaringBitmap -> IO Word64
roaringBitmapPortableSizeInBytes rbm =
    withForeignPtr (unRoaringBitmap rbm) $ \ptr ->
        coerce <$> c_roaring_bitmap_portable_size_in_bytes ptr

roaringBitmapPortableSerialize :: RoaringBitmap -> IO ByteString
roaringBitmapPortableSerialize rbm =
    withForeignPtr (unRoaringBitmap rbm) $ \ptr -> do
        size <- fromIntegral <$> c_roaring_bitmap_portable_size_in_bytes ptr
        allocaArray size $ \ans -> do
            c_roaring_bitmap_portable_serialize ptr ans
            packCStringLen (ans, size)

roaringBitmapEquals :: RoaringBitmap -> RoaringBitmap -> IO Bool
roaringBitmapEquals = with2RoaringBitmaps $
    \ptr1 ptr2 -> toBool <$> c_roaring_bitmap_equals ptr1 ptr2

someFunc :: IO ()
someFunc = do
    rbmFromRange <- roaringBitmapFromRange 0 100 10
    print =<< roaringBitmapToUint32Array rbmFromRange
    rbmOfPtr <- roaringBitmapOfPtr [0..20]
    print =<< roaringBitmapToUint32Array rbmOfPtr
    intersect <- roaringBitmapAnd rbmFromRange rbmOfPtr
    print =<< roaringBitmapToUint32Array intersect
    union <- roaringBitmapOrManyHeap [rbmFromRange, rbmOfPtr, intersect]
    print =<< roaringBitmapToUint32Array union
    roaringBitmapAndInplace rbmFromRange rbmOfPtr
    print =<< roaringBitmapToUint32Array rbmFromRange
    rbmFull <- roaringBitmapFromRange 0 (2^32) 1
    rbmAlmostFull <- roaringBitmapFromRange 1 (2^33) 1
    diff <- roaringBitmapXor rbmFull rbmAlmostFull
    print =<< roaringBitmapToUint32Array diff
    serialised <- roaringBitmapPortableSerialize union
    deserialised <- roaringBitmapPortableDeserialize serialised
    print =<< roaringBitmapEquals deserialised union
    pure ()
