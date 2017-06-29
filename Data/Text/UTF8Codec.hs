{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}

module Data.Text.UTF8Codec where

import Data.Primitive.ByteArray
import Data.Array
import GHC.Prim
import GHC.Types
import GHC.ST
import GHC.Word

-- | Encode a 'Char' into bytes
--
-- Write @\U+FFFD@ (encoded as @EF BF BD@ 3 bytes) for invalid unicode codepoint.
-- This function assumed there're enough space for encoded bytes, and return the advanced index.
--
encodeChar :: MutablePrimArray s Word8 -> Int -> Char -> ST s Int
{-# INLINE encodeChar #-}
encodeChar (MutablePrimArray (MutableByteArray mba#)) (I# i#) (C# c#) = ST (\ s# ->
    let (# s1#, j# #) = encodeChar# mba# i# c# s# in (# s1#, (I# j#) #))

-- | The unboxed version of 'encodeChar'
--
-- This function is marked as @NOINLINE@ to reduce code size.
--
encodeChar# :: MutableByteArray# s -> Int# -> Char# -> State# s -> (# State# s, Int# #)
{-# NOINLINE encodeChar# #-} -- codesize vs speed choice here
encodeChar# mba# i# c# = case (int2Word# (ord# c#)) of
    n#
        | isTrue# (n# `leWord#` 0x0000007F##) -> \ s# ->
            let s1# = writeWord8Array# mba# i# n# s#
            in (# s1#, i# +# 1# #)
        | isTrue# (n# `leWord#` 0x000007FF##) -> \ s# ->
            let s1# = writeWord8Array# mba# i# (0xC0## `or#` (n# `uncheckedShiftRL#` 6#)) s#
                s2# = writeWord8Array# mba# (i# +# 1#) (0x80## `or#` (n# `and#` 0x3F##)) s1#
            in (# s2#, i# +# 2# #)
        | isTrue# (n# `leWord#` 0x0000D7FF##) -> \ s# ->
            let s1# = writeWord8Array# mba# i# (0xE0## `or#` (n# `uncheckedShiftRL#` 12#)) s#
                s2# = writeWord8Array# mba# (i# +# 1#) (0x80## `or#` ((n# `uncheckedShiftRL#` 6#) `and#` 0x3F##)) s1#
                s3# = writeWord8Array# mba# (i# +# 2#) (0x80## `or#` (n# `and#` 0x3F##)) s2#
            in (# s3#, i# +# 3# #)
        | isTrue# (n# `leWord#` 0x0000DFFF##) -> \ s# -> -- write replacement char \U+FFFD
            let s1# = writeWord8Array# mba# i# 0xEF## s#
                s2# = writeWord8Array# mba# (i# +# 1#) 0xBF## s1#
                s3# = writeWord8Array# mba# (i# +# 2#) 0xBD## s2#
            in (# s3#, i# +# 3# #)
        | isTrue# (n# `leWord#` 0x0010FFFF##) -> \ s# ->
            let s1# = writeWord8Array# mba# i# (0xF0## `or#` (n# `uncheckedShiftRL#` 18#)) s#
                s2# = writeWord8Array# mba# (i# +# 1#) (0x80## `or#` ((n# `uncheckedShiftRL#` 12#) `and#` 0x3F##)) s1#
                s3# = writeWord8Array# mba# (i# +# 2#) (0x80## `or#` ((n# `uncheckedShiftRL#` 6#) `and#` 0x3F##)) s2#
                s4# = writeWord8Array# mba# (i# +# 3#) (0x80## `or#` (n# `and#` 0x3F##)) s3#
            in (# s4#, i# +# 4# #)
        | otherwise -> \ s# -> -- write replacement char \U+FFFD
            let s1# = writeWord8Array# mba# i#  0xEF## s#
                s2# = writeWord8Array# mba# (i# +# 1#) 0xBF## s1#
                s3# = writeWord8Array# mba# (i# +# 2#) 0xBD## s2#
            in (# s3#, i# +# 3# #)

-- | Decode a 'Char' from bytes
--
-- This function assumed all bytes are UTF-8 encoded, and the index param point to the
-- beginning of a codepoint, the decoded character and the advancing offset are returned.
--
-- It's annoying to use unboxed tuple here but we really want GHC to optimize it away.
--
decodeChar :: PrimArray Word8 -> Int -> (# Char, Int #)
{-# INLINE decodeChar #-}
decodeChar (PrimArray (ByteArray ba#)) (I# idx#) =
    let (# c#, i# #) = decodeChar# ba# idx# in (# C# c#, I# i# #)

-- | The unboxed version of 'decodeChar'
--
-- This function is marked as @NOINLINE@ to reduce code size.
--
decodeChar# :: ByteArray# -> Int# -> (# Char#, Int# #)
{-# NOINLINE decodeChar# #-} -- This branchy code make GHC impossible to fuse, DON'T inline
decodeChar# ba# idx#
    | isTrue# (w1# `leWord#` 0x7F##) = (# chr1# w1#, 1# #)
    | isTrue# (w1# `leWord#` 0xDF##) =
        let w2# = indexWord8Array# ba# (idx# +# 1#)
        in (# chr2# w1# w2#, 2# #)
    | isTrue# (w1# `leWord#` 0xEF##) =
        let w2# = indexWord8Array# ba# (idx# +# 1#)
            w3# = indexWord8Array# ba# (idx# +# 2#)
        in (# chr3# w1# w2# w3#, 3# #)
    | otherwise =
        let w2# = indexWord8Array# ba# (idx# +# 1#)
            w3# = indexWord8Array# ba# (idx# +# 2#)
            w4# = indexWord8Array# ba# (idx# +# 3#)
        in (# chr4# w1# w2# w3# w4#, 4# #)
  where
    w1# = indexWord8Array# ba# idx#

-- | Decode a 'Char' from bytes in rerverse order.
--
-- This function assumed all bytes are UTF-8 encoded, and the index param point to the end
-- of a codepoint, the decoded character and the backward advancing offset are returned.
--
decodeCharReverse :: PrimArray Word8 -> Int -> (# Char, Int #)
{-# INLINE decodeCharReverse #-}
decodeCharReverse (PrimArray (ByteArray ba#)) (I# idx#) =
    let (# c#, i# #) = decodeCharReverse# ba# idx# in (# C# c#, I# i# #)

-- | The unboxed version of 'decodeCharReverse'
--
-- This function is marked as @NOINLINE@ to reduce code size.
--
decodeCharReverse# :: ByteArray# -> Int# -> (# Char#, Int# #)
{-# NOINLINE decodeCharReverse# #-} -- This branchy code make GHC impossible to fuse, DON'T inline
decodeCharReverse# ba# idx# =
    let w1# = indexWord8Array# ba# idx#
    in if isContinueByte# w1#
        then
        let w2# = indexWord8Array# ba# (idx# -# 1#)
        in if isContinueByte# w2#
        then
            let w3# = indexWord8Array# ba# (idx# -# 2#)
            in if isContinueByte# w3#
            then
                let w4# = indexWord8Array# ba# (idx# -# 3#)
                in (# chr4# w4# w3# w2# w1#, 4# #)
            else (# chr3# w3# w2# w1#, 3# #)
        else  (# chr2# w2# w1#, 2# #)
    else (# chr1# w1#, 1# #)

-- | Validate if current index point to a valid utf8 codepoint.
-- If the does, return the utf8 bytes length, otherwise return the negation of
-- offset we should skip(so that a replacing decoder can meet the security rules).
-- If @0#@ is returned, then you should feed more bytes to continue validation.
--
-- reference: https://howardhinnant.github.io/utf_summary.html
--
validateChar# :: ByteArray# -> Int# -> Int# -> Int#
{-# NOINLINE validateChar# #-}
validateChar# ba# idx# end# =
    case end# -# idx# of
        1#
            | isTrue# (w1# `leWord#` 0x7F##) -> 1#
            | isTrue# (w1# `leWord#` 0xC1##) -> -1#
            | isTrue# (w1# `geWord#` 0xF5##) -> -1#
            | otherwise -> 0#
        2#
            | isTrue# (w1# `leWord#` 0x7F##) -> 1#
            | isTrue# (w1# `leWord#` 0xC1##) -> -1#
            | isTrue# (w1# `leWord#` 0xDF##) ->
                let w2# = indexWord8Array# ba# (idx# +# 1#)
                in if between# w2# 0x80## 0xBF##
                then 2#
                else -1#
            | isTrue# (w1# `geWord#` 0xF5##) -> -1#
            | otherwise -> 0#
        3#
            | isTrue# (w1# `leWord#` 0x7F##) -> 1#
            | isTrue# (w1# `leWord#` 0xC1##) -> -1#
            | isTrue# (w1# `leWord#` 0xDF##) ->
                let w2# = indexWord8Array# ba# (idx# +# 1#)
                in if between# w2# 0x80## 0xBF##
                then 2#
                else -1#

            | isTrue# (w1# `eqWord#` 0xE0##) ->
                let w2# = indexWord8Array# ba# (idx# +# 1#)
                    w3# = indexWord8Array# ba# (idx# +# 2#)
                in if between# w2# 0xA0## 0xBF##
                then if between# w3# 0x80## 0xBF##
                    then 3#
                    else -2#
                else -1#

            | isTrue# (w1# `leWord#` 0xEC##) ->
                let w2# = indexWord8Array# ba# (idx# +# 1#)
                    w3# = indexWord8Array# ba# (idx# +# 2#)
                in if between# w2# 0x80## 0xBF##
                then if between# w3# 0x80## 0xBF##
                    then 3#
                    else -2#
                else -1#

            | isTrue# (w1# `eqWord#` 0xED##) ->
                let w2# = indexWord8Array# ba# (idx# +# 1#)
                    w3# = indexWord8Array# ba# (idx# +# 2#)
                in if between# w2# 0x80## 0x9F##
                then if between# w3# 0x80## 0xBF##
                    then 3#
                    else -2#
                else -1#
            | isTrue# (w1# `eqWord#` 0xEF##) ->
                let w2# = indexWord8Array# ba# (idx# +# 1#)
                    w3# = indexWord8Array# ba# (idx# +# 2#)
                in if between# w2# 0x80## 0xBF##
                then if between# w3# 0x80## 0xBF##
                    then 3#
                    else -2#
                else -1#
            | isTrue# (w1# `geWord#` 0xF5##) -> -1#
            | otherwise -> 0#

        _
            | isTrue# (w1# `leWord#` 0x7F##) -> 1#
            | isTrue# (w1# `leWord#` 0xC1##) -> -1#
            | isTrue# (w1# `leWord#` 0xDF##) ->
                let w2# = indexWord8Array# ba# (idx# +# 1#)
                in if between# w2# 0x80## 0xBF##
                then 2#
                else -1#

            | isTrue# (w1# `eqWord#` 0xE0##) ->
                let w2# = indexWord8Array# ba# (idx# +# 1#)
                    w3# = indexWord8Array# ba# (idx# +# 2#)
                in if between# w2# 0xA0## 0xBF##
                then if between# w3# 0x80## 0xBF##
                    then 3#
                    else -2#
                else -1#

            | isTrue# (w1# `leWord#` 0xEC##) ->
                let w2# = indexWord8Array# ba# (idx# +# 1#)
                    w3# = indexWord8Array# ba# (idx# +# 2#)
                in if between# w2# 0x80## 0xBF##
                then if between# w3# 0x80## 0xBF##
                    then 3#
                    else -2#
                else -1#

            | isTrue# (w1# `eqWord#` 0xED##) ->
                let w2# = indexWord8Array# ba# (idx# +# 1#)
                    w3# = indexWord8Array# ba# (idx# +# 2#)
                in if between# w2# 0x80## 0x9F##
                then if between# w3# 0x80## 0xBF##
                    then 3#
                    else -2#
                else -1#
            | isTrue# (w1# `eqWord#` 0xEF##) ->
                let w2# = indexWord8Array# ba# (idx# +# 1#)
                    w3# = indexWord8Array# ba# (idx# +# 2#)
                in if between# w2# 0x80## 0xBF##
                then if between# w3# 0x80## 0xBF##
                    then 3#
                    else -2#
                else -1#

            | isTrue# (w1# `eqWord#` 0xF0##) ->
                let w2# = indexWord8Array# ba# (idx# +# 1#)
                    w3# = indexWord8Array# ba# (idx# +# 2#)
                    w4# = indexWord8Array# ba# (idx# +# 3#)
                in if between# w2# 0x90## 0xBF##
                then if between# w3# 0x80## 0xBF##
                    then if between# w4# 0x80## 0xBF##
                        then 4#
                        else -3#
                    else -2#
                else -1#

            | isTrue# (w1# `leWord#` 0xF3##) ->
                let w2# = indexWord8Array# ba# (idx# +# 1#)
                    w3# = indexWord8Array# ba# (idx# +# 2#)
                    w4# = indexWord8Array# ba# (idx# +# 3#)
                in if between# w2# 0x80## 0xBF##
                then if between# w3# 0x80## 0xBF##
                    then if between# w4# 0x80## 0xBF##
                        then 4#
                        else -3#
                    else -2#
                else -1#

            | isTrue# (w1# `eqWord#` 0xF4##) ->
                let w2# = indexWord8Array# ba# (idx# +# 1#)
                    w3# = indexWord8Array# ba# (idx# +# 2#)
                    w4# = indexWord8Array# ba# (idx# +# 3#)
                in if between# w2# 0x80## 0x8F##
                then if between# w3# 0x80## 0xBF##
                    then if between# w4# 0x80## 0xBF##
                        then 4#
                        else -3#
                    else -2#
                else -1#
            | isTrue# (w1# `geWord#` 0xF5##) -> -1#
            | otherwise -> -1#
  where
    w1# = indexWord8Array# ba# idx#

between# :: Word# -> Word# -> Word# -> Bool
{-# INLINE between# #-}
between# w# l# h# = isTrue# (w# `geWord#` l#) && isTrue# (w# `leWord#` h#)

isContinueByte# :: Word# -> Bool
{-# INLINE isContinueByte# #-}
isContinueByte# w# = isTrue# (and# w# 0xC0## `eqWord#` 0x80##)

chr1# :: Word# -> Char#
{-# INLINE chr1# #-}
chr1# x1# = chr# y1#
  where
    !y1# = word2Int# x1#

chr2# :: Word# -> Word# -> Char#
{-# INLINE chr2# #-}
chr2# x1# x2# = chr# (z1# +# z2#)
  where
    !y1# = word2Int# x1#
    !y2# = word2Int# x2#
    !z1# = uncheckedIShiftL# (y1# -# 0xC0#) 6#
    !z2# = y2# -# 0x80#

chr3# :: Word# -> Word# -> Word# -> Char#
{-# INLINE chr3# #-}
chr3# x1# x2# x3# = chr# (z1# +# z2# +# z3#)
  where
    !y1# = word2Int# x1#
    !y2# = word2Int# x2#
    !y3# = word2Int# x3#
    !z1# = uncheckedIShiftL# (y1# -# 0xE0#) 12#
    !z2# = uncheckedIShiftL# (y2# -# 0x80#) 6#
    !z3# = y3# -# 0x80#

chr4# :: Word# -> Word# -> Word# -> Word# -> Char#
{-# INLINE chr4# #-}
chr4# x1# x2# x3# x4# = chr# (z1# +# z2# +# z3# +# z4#)
  where
    !y1# = word2Int# x1#
    !y2# = word2Int# x2#
    !y3# = word2Int# x3#
    !y4# = word2Int# x4#
    !z1# = uncheckedIShiftL# (y1# -# 0xF0#) 18#
    !z2# = uncheckedIShiftL# (y2# -# 0x80#) 12#
    !z3# = uncheckedIShiftL# (y3# -# 0x80#) 6#
    !z4# = y4# -# 0x80#
