{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

-- | This module provide fast memchr implemented with ghc primitives.
--
-- http://lemire.me/blog/2017/01/20/how-quickly-can-you-remove-spaces-from-a-string/
-- https://graphics.stanford.edu/~seander/bithacks.html
-- https://jameshfisher.github.io/2017/01/24/bitwise-check-for-zero-byte.html
--
--
module Data.Primitive.BitTwiddle where

import GHC.Prim
import GHC.Types
import Debug.Trace

-- we need to know word size
#include "MachDeps.h"

#if SIZEOF_HSWORD == 4
# define CAST_OFFSET_WORD_TO_BYTE(x) (x `uncheckedIShiftL#` 2#)
# define CAST_OFFSET_BYTE_TO_WORD(x) (x `uncheckedIShiftRA#` 2#)
#else
# define CAST_OFFSET_WORD_TO_BYTE(x) (x `uncheckedIShiftL#` 3#)
# define CAST_OFFSET_BYTE_TO_WORD(x) (x `uncheckedIShiftRA#` 3#)
#endif

isOffsetAlign# :: Int# -> Bool
{-# INLINE isOffsetAlign# #-}
isOffsetAlign# s# = isTrue# ((SIZEOF_HSWORD# -# 1#) `andI#` s# ==# 0#)

mkMask# :: Word# -> Word#
{-# INLINE mkMask# #-}
mkMask# w8# =
#if SIZEOF_HSWORD == 4
    w8# `or#` (w8# `uncheckedShiftL#` 8#)
        `or#` (w8# `uncheckedShiftL#` 16#)
        `or#` (w8# `uncheckedShiftL#` 24#)
#else
    w8# `or#` (w8# `uncheckedShiftL#` 8#)
        `or#` (w8# `uncheckedShiftL#` 16#)
        `or#` (w8# `uncheckedShiftL#` 24#)
        `or#` (w8# `uncheckedShiftL#` 32#)
        `or#` (w8# `uncheckedShiftL#` 40#)
        `or#` (w8# `uncheckedShiftL#` 48#)
        `or#` (w8# `uncheckedShiftL#` 56#)
#endif

-- https://jameshfisher.github.io/2017/01/24/bitwise-check-for-zero-byte.html
--
wordNotContainNullByte# :: Word# -> Bool
{-# INLINE wordNotContainNullByte# #-}
wordNotContainNullByte# w# =
    let highbits# =
#if SIZEOF_HSWORD == 4
            (w# `minusWord#` 0x01010101##) `and#` (not# w#) `and#` 0x80808080##
#else
            (w# `minusWord#` 0x0101010101010101##) `and#` (not# w#) `and#` 0x8080808080808080##
#endif
    in isTrue# (highbits# `eqWord#` 0##)

--
-- https://sourceware.org/viewvc/src/newlib/libc/string/memchr.c?revision=1.4&view=markup

memchr# :: ByteArray# -> Word# -> Int# -> Int# -> Int#
{-# INLINE memchr# #-}
memchr# = beforeAlignedLoop#

beforeAlignedLoop# :: ByteArray# -> Word# -> Int# -> Int# -> Int#
beforeAlignedLoop# ba# c# s# end#
    | isTrue# (s# >=# end#) = -1#
    | isTrue# (c# `eqWord#` indexWord8Array# ba# s#) = s#
    | isOffsetAlign# s#     = alignedLoop# ba# c# (mkMask# c#)
                                               CAST_OFFSET_BYTE_TO_WORD(s#)
                                               CAST_OFFSET_BYTE_TO_WORD(end#)
                                               end#
    | otherwise = beforeAlignedLoop# ba# c# (s# +# 1#) end#

alignedLoop# :: ByteArray# -> Word# -> Word# -> Int# -> Int# -> Int# -> Int#
alignedLoop# ba# c# mask# s# end# end_#
    | isTrue# (s# >=# end#) = afterAlignedLoop# ba# c# CAST_OFFSET_WORD_TO_BYTE(s#) end_#
    | otherwise = case indexWordArray# ba# s# of
        w#
            | wordNotContainNullByte# (mask# `xor#` w#) ->
                alignedLoop# ba# c# mask# (s# +# 1#) end# end_#
            | otherwise ->
                afterAlignedLoop# ba# c# CAST_OFFSET_WORD_TO_BYTE(s#) end_#

afterAlignedLoop# :: ByteArray# -> Word# -> Int# -> Int# -> Int#
afterAlignedLoop# ba# c# s# end#
    | isTrue# (s# >=# end#) = -1#
    | isTrue# (c# `eqWord#` indexWord8Array# ba# s#) = s#
    | otherwise = afterAlignedLoop# ba# c# (s# +# 1#) end#

