{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module Harimau.Bindings where

import Foreign
import Foreign.C.Types

data RawRoaringBitmap

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

foreign import ccall safe "roaring.h roaring_bitmap_intersect_with_range"
    c_roaring_bitmap_intersect_with_range :: Ptr RawRoaringBitmap -> CULong -> CULong -> IO CBool

foreign import ccall safe "roaring.h roaring_bitmap_jaccard_index"
    c_roaring_bitmap_jaccard_index :: Ptr RawRoaringBitmap -> Ptr RawRoaringBitmap -> IO CDouble

foreign import ccall safe "roaring.h roaring_bitmap_or_cardinality"
    c_roaring_bitmap_or_cardinality :: Ptr RawRoaringBitmap -> Ptr RawRoaringBitmap -> IO CULong

foreign import ccall safe "roaring.h roaring_bitmap_andnot_cardinality"
    c_roaring_bitmap_andnot_cardinality :: Ptr RawRoaringBitmap -> Ptr RawRoaringBitmap -> IO CULong

foreign import ccall safe "roaring.h roaring_bitmap_xor_cardinality"
    c_roaring_bitmap_xor_cardinality :: Ptr RawRoaringBitmap -> Ptr RawRoaringBitmap -> IO CULong

foreign import ccall safe "roaring.h roaring_bitmap_and_inplace"
    c_roaring_bitmap_and_inplace :: Ptr RawRoaringBitmap -> Ptr RawRoaringBitmap -> IO ()

foreign import ccall safe "roaring.h roaring_bitmap_or"
    c_roaring_bitmap_or :: Ptr RawRoaringBitmap -> Ptr RawRoaringBitmap -> IO (Ptr RawRoaringBitmap)

foreign import ccall safe "roaring.h roaring_bitmap_or_inplace"
    c_roaring_bitmap_or_inplace :: Ptr RawRoaringBitmap -> Ptr RawRoaringBitmap -> IO ()

foreign import ccall safe "roaring.h roaring_bitmap_or_many"
    c_roaring_bitmap_or_many :: CSize -> Ptr (Ptr RawRoaringBitmap) -> IO (Ptr RawRoaringBitmap)

foreign import ccall safe "roaring.h roaring_bitmap_or_many_heap"
    c_roaring_bitmap_or_many_heap :: CSize -> Ptr (Ptr RawRoaringBitmap) -> IO (Ptr RawRoaringBitmap)

foreign import ccall safe "roaring.h roaring_bitmap_xor"
    c_roaring_bitmap_xor :: Ptr RawRoaringBitmap -> Ptr RawRoaringBitmap -> IO (Ptr RawRoaringBitmap)

foreign import ccall safe "roaring.h roaring_bitmap_xor_inplace"
    c_roaring_bitmap_xor_inplace :: Ptr RawRoaringBitmap -> Ptr RawRoaringBitmap -> IO ()

foreign import ccall safe "roaring.h roaring_bitmap_xor_many"
    c_roaring_bitmap_xor_many :: CSize -> Ptr (Ptr RawRoaringBitmap) -> IO (Ptr RawRoaringBitmap)

foreign import ccall safe "roaring.h roaring_bitmap_andnot"
    c_roaring_bitmap_andnot :: Ptr RawRoaringBitmap -> Ptr RawRoaringBitmap -> IO (Ptr RawRoaringBitmap)

foreign import ccall safe "roaring.h roaring_bitmap_andnot_inplace"
    c_roaring_bitmap_andnot_inplace :: Ptr RawRoaringBitmap -> Ptr RawRoaringBitmap -> IO ()

foreign import ccall safe "roaring.h roaring_bitmap_add_many"
    c_roaring_bitmap_add_many :: Ptr RawRoaringBitmap -> CSize -> Ptr CUInt -> IO ()

foreign import ccall safe "roaring.h roaring_bitmap_add"
    c_roaring_bitmap_add :: Ptr RawRoaringBitmap -> CUInt -> IO ()

foreign import ccall safe "roaring.h roaring_bitmap_add_checked"
    c_roaring_bitmap_add_checked :: Ptr RawRoaringBitmap -> CUInt -> IO CBool

foreign import ccall safe "roaring.h roaring_bitmap_add_range_closed"
    c_roaring_bitmap_add_range_closed :: Ptr RawRoaringBitmap -> CUInt -> CUInt -> IO ()

foreign import ccall safe "roaring.h roaring_bitmap_add_range"
    c_roaring_bitmap_add_range :: Ptr RawRoaringBitmap -> CULong -> CULong -> IO ()

foreign import ccall safe "roaring.h roaring_bitmap_remove"
    c_roaring_bitmap_remove :: Ptr RawRoaringBitmap -> CUInt -> IO ()

foreign import ccall safe "roaring.h roaring_bitmap_remove_range_closed"
    c_roaring_bitmap_remove_range_closed :: Ptr RawRoaringBitmap -> CUInt -> CUInt -> IO ()

foreign import ccall safe "roaring.h roaring_bitmap_remove_range"
    c_roaring_bitmap_remove_range :: Ptr RawRoaringBitmap -> CULong -> CULong -> IO ()

foreign import ccall safe "roaring.h roaring_bitmap_remove_many"
    c_roaring_bitmap_remove_many :: Ptr RawRoaringBitmap -> CSize -> Ptr CUInt -> IO ()

foreign import ccall safe "roaring.h roaring_bitmap_remove_checked"
    c_roaring_bitmap_remove_checked :: Ptr RawRoaringBitmap -> CUInt -> IO CBool

foreign import ccall safe "roaring.h roaring_bitmap_contains"
    c_roaring_bitmap_contains :: Ptr RawRoaringBitmap -> CUInt -> IO CBool

foreign import ccall safe "roaring.h roaring_bitmap_contains_range"
    c_roaring_bitmap_contains_range :: Ptr RawRoaringBitmap -> CULong -> CULong -> IO CBool

foreign import ccall safe "roaring.h roaring_bitmap_get_cardinality"
    c_roaring_bitmap_get_cardinality :: Ptr RawRoaringBitmap -> IO CULong

foreign import ccall safe "roaring.h roaring_bitmap_is_empty"
    c_roaring_bitmap_is_empty :: Ptr RawRoaringBitmap -> IO CBool

foreign import ccall safe "roaring.h roaring_bitmap_clear"
    c_roaring_bitmap_clear :: Ptr RawRoaringBitmap -> IO ()

foreign import ccall safe "roaring.h roaring_bitmap_to_uint32_array"
    c_roaring_bitmap_to_uint32_array :: Ptr RawRoaringBitmap -> Ptr CUInt -> IO ()

foreign import ccall safe "roaring.h roaring_bitmap_range_uint32_array"
    c_roaring_bitmap_range_uint32_array :: Ptr RawRoaringBitmap -> CSize -> CSize -> Ptr CUInt -> IO ()

foreign import ccall safe "roaring.h roaring_bitmap_remove_run_compression"
    c_roaring_bitmap_remove_run_compression :: Ptr RawRoaringBitmap -> IO CBool

foreign import ccall safe "roaring.h roaring_bitmap_run_optimize"
    c_roaring_bitmap_run_optimize :: Ptr RawRoaringBitmap -> IO CBool

foreign import ccall safe "roaring.h roaring_bitmap_shrink_to_fit"
    c_roaring_bitmap_shrink_to_fit :: Ptr RawRoaringBitmap -> IO CSize

foreign import ccall safe "roaring.h roaring_bitmap_portable_deserialize"
    c_roaring_bitmap_portable_deserialize :: Ptr CChar -> IO (Ptr RawRoaringBitmap)

foreign import ccall safe "roaring.h roaring_bitmap_portable_size_in_bytes"
    c_roaring_bitmap_portable_size_in_bytes :: Ptr RawRoaringBitmap -> IO CSize

foreign import ccall safe "roaring.h roaring_bitmap_portable_serialize"
    c_roaring_bitmap_portable_serialize :: Ptr RawRoaringBitmap -> Ptr CChar -> IO CSize

foreign import ccall safe "roaring.h roaring_bitmap_equals"
    c_roaring_bitmap_equals :: Ptr RawRoaringBitmap -> Ptr RawRoaringBitmap -> IO CBool

foreign import ccall safe "roaring.h roaring_bitmap_is_subset"
    c_roaring_bitmap_is_subset :: Ptr RawRoaringBitmap -> Ptr RawRoaringBitmap -> IO CBool

foreign import ccall safe "roaring.h roaring_bitmap_is_strict_subset"
    c_roaring_bitmap_is_strict_subset :: Ptr RawRoaringBitmap -> Ptr RawRoaringBitmap -> IO CBool

foreign import ccall safe "roaring.h roaring_bitmap_flip"
    c_roaring_bitmap_flip :: Ptr RawRoaringBitmap -> CULong -> CULong -> IO (Ptr RawRoaringBitmap)

foreign import ccall safe "roaring.h roaring_bitmap_flip_inplace"
    c_roaring_bitmap_flip_inplace :: Ptr RawRoaringBitmap -> CULong -> CULong -> IO ()

foreign import ccall safe "roaring.h roaring_bitmap_select"
    c_roaring_bitmap_select :: Ptr RawRoaringBitmap -> CUInt -> Ptr CUInt -> IO CBool

foreign import ccall safe "roaring.h roaring_bitmap_rank"
    c_roaring_bitmap_rank :: Ptr RawRoaringBitmap -> CUInt -> IO CULong

foreign import ccall safe "roaring.h roaring_bitmap_get_index"
    c_roaring_bitmap_get_index :: Ptr RawRoaringBitmap -> CUInt -> IO CLong

foreign import ccall safe "roaring.h roaring_bitmap_minimum"
    c_roaring_bitmap_minimum :: Ptr RawRoaringBitmap -> IO CUInt

foreign import ccall safe "roaring.h roaring_bitmap_maximum"
    c_roaring_bitmap_maximum :: Ptr RawRoaringBitmap -> IO CUInt
