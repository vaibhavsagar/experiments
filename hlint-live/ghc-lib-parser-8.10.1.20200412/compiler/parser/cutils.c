/*
These utility routines are used various
places in the GHC library.
*/

#include <Rts.h>

#include <HsFFI.h>

void
ghc_lib_parser_enableTimingStats( void )       /* called from the driver */
{
    RtsFlags.GcFlags.giveStats = ONELINE_GC_STATS;
}

void
ghc_lib_parser_setHeapSize( HsInt size )
{
    RtsFlags.GcFlags.heapSizeSuggestion = size / BLOCK_SIZE;
    if (RtsFlags.GcFlags.maxHeapSize != 0 &&
        RtsFlags.GcFlags.heapSizeSuggestion > RtsFlags.GcFlags.maxHeapSize) {
        RtsFlags.GcFlags.maxHeapSize = RtsFlags.GcFlags.heapSizeSuggestion;
    }
}
