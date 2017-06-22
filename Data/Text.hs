{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}

module Data.Text where

import GHC.Prim
import GHC.Types
import GHC.CString
import GHC.Exts (IsString(..))
import Data.Primitive.ByteArray
import qualified Data.Vector as V
import qualified Data.Primitive.PrimArray as PA
import Control.Monad.ST
import Data.Word
import Data.Char
import Data.Bits

-- | 'Text' represented as UTF-8 encoded 'Bytes'
--
newtype Text = Text V.Bytes

instance IsString Text where
    {-# INLINE fromString #-}
    fromString = pack

{-# RULES "Text literal" forall addr. pack (unpackCString# addr) = Text (V.newBytesFromAddr addr) #-}

{-
validateUTF8 :: Bytes -> (Bool, Int)

fromUTF8 :: Bytes -> (Text, Bytes)
fromUTF8Lenient :: Bytes -> (Text, Bytes)
toUTF8 :: Text -> Bytes

fromUTF16 :: Bytes -> (Text, Bytes)
fromUTF16Lenient :: Bytes -> (Text, Bytes)
toUTF16 :: Text -> Bytes


packN :: String -> Text
packN =
-}

pack :: String -> Text
pack = packN V.defaultInitSize
{-# INLINE [1] pack #-}

-- | /O(n)/ Convert a list into a text with an approximate size(in bytes, not codepoints).
--
-- If the list's length is large than the size given, we simply double the buffer size
-- and continue building.
--
-- This function is a /good consumer/ in the sense of build/foldr fusion.
--
packN :: Int -> String -> Text
packN n0 = \ ws0 -> runST (do mba <- newArr n0
                              (SP3 i _ mba') <- foldlM go (SP3 0 n0 mba) ws0
                              shrinkMutableArr mba' i
                              ba <- unsafeFreezeArr mba'
                              return (fromArr ba 0 i)
                          )
  where
    -- It's critical that this function get specialized and unboxed
    -- Keep an eye on its core!
    go :: SP3 s -> Char -> ST s (SP3 s)
    go (SP3 i siz mba) !c =
        if i < siz - 3  -- we need at least 4 bytes for safety
        then case ord c of
            n
                | n <= 0x0000007F -> do
                    PA.writePrimArray mba i (fromIntegral n)
                    return (SP3 (i+1) siz mba)
                | n <= 0x000007FF -> do
                    PA.writePrimArray mba i     (fromIntegral (0xC0 .|. (n `shiftR` 6)))
                    PA.writePrimArray mba (i+1) (fromIntegral (0x80 .|. (n .&. 0x3F)))
                    return (SP3 (i+2) siz mba)
                | n <= 0x0000FFFF -> do
                    PA.writePrimArray mba i     (fromIntegral (0xE0 .|. (n `shiftR` 12)))
                    PA.writePrimArray mba (i+1) (fromIntegral (0x80 .|. ((n `shiftR` 6) .&. 0x3F)))
                    PA.writePrimArray mba (i+2) (fromIntegral (0x80 .|. (n .&. 0x3F)))
                    return (SP3 (i+3) siz mba)
                | n <= 0x0010FFFF -> do
                    PA.writePrimArray mba i     (fromIntegral (0xF0 .|. (n `shiftR` 18)))
                    PA.writePrimArray mba (i+1) (fromIntegral (0x80 .|. ((n `shiftR` 12) .&. 0x3F)))
                    PA.writePrimArray mba (i+2) (fromIntegral (0x80 .|. ((n `shiftR` 6) .&. 0x3F)))
                    PA.writePrimArray mba (i+3) (fromIntegral (0x80 .|. (n .&. 0x3F)))
                    return (SP3 (i+4) siz mba)
        else do
            let !siz' = (siz + chunkOverhead) `shiftL` 1 - chunkOverhead
                !i' = i+1
            !mba' <- resizeMutableArr mba n'
            case ord c of
                n
                    | n <= 0x0000007F -> do
                        PA.writePrimArray mba i (fromIntegral n)
                        return (SP3 (i+1) siz' mba)
                    | n <= 0x000007FF -> do
                        PA.writePrimArray mba i     (fromIntegral (0xC0 .|. (n `shiftR` 6)))
                        PA.writePrimArray mba (i+1) (fromIntegral (0x80 .|. (n .&. 0x3F)))
                        return (SP3 (i+2) siz' mba)
                    | n <= 0x0000FFFF -> do
                        PA.writePrimArray mba i     (fromIntegral (0xE0 .|. (n `shiftR` 12)))
                        PA.writePrimArray mba (i+1) (fromIntegral (0x80 .|. ((n `shiftR` 6) .&. 0x3F)))
                        PA.writePrimArray mba (i+2) (fromIntegral (0x80 .|. (n .&. 0x3F)))
                        return (SP3 (i+3) siz' mba)
                    | n <= 0x0010FFFF -> do
                        PA.writePrimArray mba i     (fromIntegral (0xF0 .|. (n `shiftR` 18)))
                        PA.writePrimArray mba (i+1) (fromIntegral (0x80 .|. ((n `shiftR` 12) .&. 0x3F)))
                        PA.writePrimArray mba (i+2) (fromIntegral (0x80 .|. ((n `shiftR` 6) .&. 0x3F)))
                        PA.writePrimArray mba (i+3) (fromIntegral (0x80 .|. (n .&. 0x3F)))
                        return (SP3 (i+4) siz' mba)

data SP3 s = SP3 {-# UNPACK #-}!Int {-# UNPACK #-}!Int {-# UNPACK #-}!(PA.MutablePrimArray s Word8)
{-# INLINE packN #-}


unpack :: Text -> String
unpack (Text (V.BytesPat (ByteArray ba#) (I# s#) (I# l#))) = go s#
  where
    !sl# = s# +# l#
    go idx# =
        let (# chr#, l# #) = decodeChar ba# idx#
            idx'# = idx# +# l#
        in case (idx'# <=# sl#) of
            1# -> C# chr# : go idx'#
            _  -> []
{-# INLINE unpack #-}


--------------------------------------------------------------------------------

decodeChar :: ByteArray# -> Int# -> (# Char#, Int# #)
{-# INLINE decodeChar #-}
decodeChar ba# idx#
    | isTrue# (w1# `leWord#` 0x7F##) = (# chr1 w1#, 1# #)
    | isTrue# (w1# `leWord#` 0xDF##) =
        let w2# = indexWord8Array# ba# (idx# +# 1#)
        in (# chr2 w1# w2#, 2# #)
    | isTrue# (w1# `leWord#` 0xEF##) =
        let w2# = indexWord8Array# ba# (idx# +# 1#)
            w3# = indexWord8Array# ba# (idx# +# 2#)
        in (# chr3 w1# w2# w3#, 3# #)
    | otherwise =
        let w2# = indexWord8Array# ba# (idx# +# 1#)
            w3# = indexWord8Array# ba# (idx# +# 2#)
            w4# = indexWord8Array# ba# (idx# +# 3#)
        in (# chr4 w1# w2# w3# w4#, 4# #)
  where
    w1# = indexWord8Array# ba# idx#
    chr1 :: Word# -> Char#
    chr1 x1# = chr# y1#
      where
        !y1# = word2Int# x1#
    {-# INLINE chr1 #-}

    chr2 :: Word# -> Word# -> Char#
    chr2 x1# x2# = chr# (z1# +# z2#)
      where
        !y1# = word2Int# x1#
        !y2# = word2Int# x2#
        !z1# = uncheckedIShiftL# (y1# -# 0xC0#) 6#
        !z2# = y2# -# 0x80#
    {-# INLINE chr2 #-}

    chr3 :: Word# -> Word# -> Word# -> Char#
    chr3 x1# x2# x3# = chr# (z1# +# z2# +# z3#)
      where
        !y1# = word2Int# x1#
        !y2# = word2Int# x2#
        !y3# = word2Int# x3#
        !z1# = uncheckedIShiftL# (y1# -# 0xE0#) 12#
        !z2# = uncheckedIShiftL# (y2# -# 0x80#) 6#
        !z3# = y3# -# 0x80#
    {-# INLINE chr3 #-}

    chr4 :: Word# -> Word# -> Word# -> Word# -> Char#
    chr4 x1# x2# x3# x4# = chr# (z1# +# z2# +# z3# +# z4#)
      where
        !y1# = word2Int# x1#
        !y2# = word2Int# x2#
        !y3# = word2Int# x3#
        !y4# = word2Int# x4#
        !z1# = uncheckedIShiftL# (y1# -# 0xF0#) 18#
        !z2# = uncheckedIShiftL# (y2# -# 0x80#) 12#
        !z3# = uncheckedIShiftL# (y3# -# 0x80#) 6#
        !z4# = y4# -# 0x80#
    {-# INLINE chr4 #-}
