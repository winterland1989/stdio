{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.Text where

import GHC.Prim
import GHC.Types
import GHC.CString
import GHC.Exts (IsString(..))
import Data.Primitive.ByteArray
import qualified Data.Vector as V
import Data.Array
import Control.Monad.ST
import Control.Exception
import Control.DeepSeq (NFData(..))
import Data.Foldable (foldlM)
import Data.Word
import Data.Char
import Data.Bits
import Data.Typeable
import Foreign.C.Types

-- | 'Text' represented as UTF-8 encoded 'Bytes'
--
newtype Text = Text V.Bytes

instance Eq Text where
    (Text b1) == (Text b2) = b1 == b2
    {-# INLINE (==) #-}

instance Show Text where
    showsPrec p t = showsPrec p (unpack t)
instance NFData Text where
    rnf (Text bs) = rnf bs

data UTF8DecodeResult
    = Success !Text
    | PartialBytes !Text !V.Bytes
    | InvalidBytes !V.Bytes
  deriving (Show, Eq)

data UTF8EncodeError = UTF8EncodeError !Char deriving (Show, Typeable)
instance Exception UTF8EncodeError

data UTF8DecodeError = UTF8DecodeError !V.Bytes deriving (Show, Typeable)
instance Exception UTF8DecodeError


validateUTF8 :: V.Bytes -> UTF8DecodeResult
validateUTF8 bs@(V.PrimVector (PrimArray (ByteArray ba#)) (I# s#) (I# l#)) = go s#
  where
    end# = s# +# l#
    go :: Int# -> UTF8DecodeResult
    go i#
        | isTrue# (i# <# end#) = case validateChar# ba# i# end# of
            r#
                | isTrue# (r# ># 0#)  -> go (i# +# r#)
                | isTrue# (r# ==# 0#) ->
                    PartialBytes
                        (Text (V.PrimVector (PrimArray (ByteArray ba#)) (I# s#) (I# (i# -# s#))))
                        (V.PrimVector (PrimArray (ByteArray ba#)) (I# i#) (I# (end# -# i#)))
                | otherwise ->
                    InvalidBytes
                        (V.PrimVector (PrimArray (ByteArray ba#)) (I# i#) (I# (negateInt# r#)))
        | otherwise = Success (Text bs)
{-# INLINE validateUTF8 #-}

validateUTF8_ :: V.Bytes -> (Text, V.Bytes)
validateUTF8_ = undefined

-- |
-- https://stackoverflow.com/questions/2547262/why-is-python-decode-replacing-more-than-the-invalid-bytes-from-an-encoded-strin

repairUTF8 :: V.Bytes -> (Text, V.Bytes)
repairUTF8 = undefined

{-
fromUTF8 :: Bytes -> (Text, Bytes)
fromUTF8Lenient :: Bytes -> (Text, Bytes)
toUTF8 :: Text -> Bytes

fromUTF16 :: Bytes -> (Text, Bytes)
fromUTF16Lenient :: Bytes -> (Text, Bytes)
toUTF16 :: Text -> Bytes

-}

--------------------------------------------------------------------------------

pack :: String -> Text
pack = packN V.defaultInitSize
{-# INLINE pack #-}

-- | /O(n)/ Convert a list into a text with an approximate size(in bytes, not codepoints).
--
-- If the encoded bytes length is larger than the size given, we simply double the buffer size
-- and continue building.
--
-- This function is a /good consumer/ in the sense of build/foldr fusion.
--
packN :: Int -> String -> Text
{-# INLINE packN #-}
packN n0 = \ ws0 -> runST (do mba <- newArr n0
                              (SP2 i mba') <- foldlM go (SP2 0 mba) ws0
                              shrinkMutableArr mba' i
                              ba <- unsafeFreezeArr mba'
                              return (Text (V.fromArr ba 0 i))
                          )
  where
    -- It's critical that this function get specialized and unboxed
    -- Keep an eye on its core!
    go :: SP2 s -> Char -> ST s (SP2 s)
    go (SP2 i mba) !c = do
        siz <- sizeofMutableArr mba
        if i < siz - 3  -- we need at least 4 bytes for safety
        then do
            i' <- encodeChar mba i c
            return (SP2 i' mba)
        else do
            let !siz' = (siz + V.chunkOverhead) `shiftL` 1 - V.chunkOverhead
            !mba' <- resizeMutableArr mba siz'
            i' <- encodeChar mba' i c
            return (SP2 i' mba')

data SP2 s = SP2 {-# UNPACK #-}!Int {-# UNPACK #-}!(MutablePrimArray s Word8)


unpack :: Text -> String
{-# INLINE unpack #-}
unpack (Text v) = go s#
  where
    !(V.PrimVector (PrimArray (ByteArray ba#)) (I# s#) (I# l#)) = v
    !sl# = s# +# l#
    go idx# =
        let (# c#, l# #) = decodeChar# ba# idx#
            idx'# = idx# +# l#
        in if isTrue# (idx'# <=# sl#) then C# c# : go idx'# else []

singleton :: Char -> Text
{-# INLINABLE singleton #-}
singleton c = Text $ V.createN 4 $ \ mba -> encodeChar mba 0 c

empty :: Text
{-# INLINABLE empty #-}
empty = Text V.empty

--------------------------------------------------------------------------------
-- * Basic interface

cons :: Char -> Text -> Text
{-# INLINABLE cons #-}
cons c (Text (V.PrimVector ba s l)) = Text $ V.createN (4 + l) $ \ mba -> do
        i <- encodeChar mba 0 c
        copyArr mba i ba s l
        return $! i + l

snoc :: Text -> Char -> Text
{-# INLINABLE snoc #-}
snoc (Text (V.PrimVector ba s l)) c = Text $ V.createN (4 + l) $ \ mba -> do
    copyArr mba 0 ba s l
    encodeChar mba l c

append :: Text -> Text -> Text
{-# INLINABLE append #-}
append t1@(Text (V.PrimVector ba1 s1 l1)) t2@(Text (V.PrimVector ba2 s2 l2))
    | l1 == 0   = t2
    | l2 == 0   = t1
    | otherwise = Text $ V.create (l1 + l2) $ \ mba -> do
        copyArr mba 0 ba1 s1 l1
        copyArr mba l1 ba2 s2 l2

uncons :: Text -> Maybe (Char, Text)
{-# INLINABLE uncons #-}
uncons (Text v)
    | l == 0  = Nothing
    | otherwise =
        let (# c#, i# #) = decodeChar# ba# s#
        in Just (C# c#, Text (V.PrimVector ba (I# (s# +# i#)) (I# (l# -# i#))))
  where
    !(V.PrimVector ba@(PrimArray (ByteArray ba#)) (I# s#) l@(I# l#)) = v
{-
head
last
tail
init
null
length
compareLength
-}
--------------------------------------------------------------------------------

utf8CharLength :: Char -> Int
{-# INLINE utf8CharLength #-}
utf8CharLength n
    | n <= '\x00007F' = 1
    | n <= '\x0007FF' = 2
    | n <= '\x00FFFF' = 3
    | n <= '\x10FFFF' = 4

-- | Encode a 'Char' into bytes, throw 'UTF8EncodeError' for invalid unicode codepoint.
--
encodeChar :: MutablePrimArray s Word8 -> Int -> Char -> ST s Int
{-# INLINE encodeChar #-}
encodeChar !mba !i !c
    | n <= 0x0000007F = do
        writeArr mba i (fromIntegral n)
        return $! i+1
    | n <= 0x000007FF = do
        writeArr mba i     (fromIntegral (0xC0 .|. (n `shiftR` 6)))
        writeArr mba (i+1) (fromIntegral (0x80 .|. (n .&. 0x3F)))
        return $! i+2
    | n <= 0x0000D7FF = do
        writeArr mba i     (fromIntegral (0xE0 .|. (n `shiftR` 12)))
        writeArr mba (i+1) (fromIntegral (0x80 .|. ((n `shiftR` 6) .&. 0x3F)))
        writeArr mba (i+2) (fromIntegral (0x80 .|. (n .&. 0x3F)))
        return $! i+3
    | n <= 0x0000DFFF = throw (UTF8EncodeError c)

    | n <= 0x0010FFFF = do
        writeArr mba i     (fromIntegral (0xF0 .|. (n `shiftR` 18)))
        writeArr mba (i+1) (fromIntegral (0x80 .|. ((n `shiftR` 12) .&. 0x3F)))
        writeArr mba (i+2) (fromIntegral (0x80 .|. ((n `shiftR` 6) .&. 0x3F)))
        writeArr mba (i+3) (fromIntegral (0x80 .|. (n .&. 0x3F)))
        return $! i+4
    | otherwise = throw (UTF8EncodeError c)
  where
    !n = ord c

decodeChar# :: ByteArray# -> Int# -> (# Char#, Int# #)
{-# INLINE decodeChar# #-}
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

between# :: Word# -> Word# -> Word# -> Bool
{-# INLINE between# #-}
between# w# l# h# = isTrue# (w# `geWord#` l#) && isTrue# (w# `leWord#` h#)

-- | Validate if current index point to a valid utf8 codepoint.
-- If the does, return the utf8 bytes length, otherwise return the negation of
-- offset we should skip(so that a replacing decoder can meet the security rules).
-- If @0#@ is returned, then you should feed more bytes to continue validation.
--
-- reference: https://howardhinnant.github.io/utf_summary.html
--
validateChar# :: ByteArray# -> Int# -> Int# -> Int#
{-# INLINE validateChar# #-}
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

chr1# :: Word# -> Char#
chr1# x1# = chr# y1#
  where
    !y1# = word2Int# x1#
{-# INLINE chr1# #-}

chr2# :: Word# -> Word# -> Char#
chr2# x1# x2# = chr# (z1# +# z2#)
  where
    !y1# = word2Int# x1#
    !y2# = word2Int# x2#
    !z1# = uncheckedIShiftL# (y1# -# 0xC0#) 6#
    !z2# = y2# -# 0x80#
{-# INLINE chr2# #-}

chr3# :: Word# -> Word# -> Word# -> Char#
chr3# x1# x2# x3# = chr# (z1# +# z2# +# z3#)
  where
    !y1# = word2Int# x1#
    !y2# = word2Int# x2#
    !y3# = word2Int# x3#
    !z1# = uncheckedIShiftL# (y1# -# 0xE0#) 12#
    !z2# = uncheckedIShiftL# (y2# -# 0x80#) 6#
    !z3# = y3# -# 0x80#
{-# INLINE chr3# #-}

chr4# :: Word# -> Word# -> Word# -> Word# -> Char#
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
{-# INLINE chr4# #-}
