{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.Text where

import GHC.Prim
import GHC.Types
import GHC.CString
import GHC.Exts (IsString(..), build)
import Data.Primitive.ByteArray
import qualified Data.Vector as V
import Data.Array
import GHC.ST
import Control.Exception
import Control.DeepSeq (NFData(..))
import Data.Foldable (foldlM)
import Data.Word
import Data.Char
import Data.Bits
import qualified Data.List as List
import Data.Typeable
import Foreign.C.Types
import Data.Text.UTF8Codec

import Prelude hiding (reverse,head,tail,last,init,null
    ,length,map,lines,foldl,foldr,unlines
    ,concat,any,take,drop,splitAt,takeWhile
    ,dropWhile,span,break,elem,filter,maximum
    ,minimum,all,concatMap,foldl1,foldr1
    ,scanl,scanl1,scanr,scanr1
    ,readFile,writeFile,appendFile,replicate
    ,getContents,getLine,putStr,putStrLn,interact
    ,zip,zipWith,unzip,notElem
    )

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

validateUTF8 :: V.Bytes -> UTF8DecodeResult
{-# INLINE validateUTF8 #-}
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
{-# NOINLINE [1] unpack #-}
unpack (Text (V.PrimVector ba s l)) = go s
  where
    !sl = s + l
    go !idx =
        let (c, l) = decodeChar ba idx
            !idx' = idx + l
        in if (idx' < sl) then c : go idx' else []

unpackFB :: Text -> (Char -> a -> a) -> a -> a
{-# INLINE [0] unpackFB #-}
unpackFB (Text (V.PrimVector ba s l)) k z = go s
  where
    !sl = s + l
    go !idx =
        let (c, l) = decodeChar ba idx
            !idx' = idx + l
        in if (idx' < sl) then c `k` go idx' else z

{-# RULES
"unpack" [~1] forall t . unpack t = build (\ k z -> unpackFB t k z)
"unpackFB" [1] forall t . unpackFB t (:) [] = unpack t
 #-}


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
{-# INLINE uncons #-}
uncons (Text (V.PrimVector ba s l))
    | l == 0  = Nothing
    | otherwise =
        let (c, i) = decodeChar ba s
        in Just (c, Text (V.PrimVector ba (s+i) (l-i)))

unsnoc :: Text -> Maybe (Text, Char)
{-# INLINE unsnoc #-}
unsnoc (Text (V.PrimVector ba s l))
    | l == 0  = Nothing
    | otherwise =
        let (c, i) = decodeCharReverse ba (s + l - 1)
        in Just (Text (V.PrimVector ba s (l-i)), c)

head :: Text -> Char
{-# INLINABLE head #-}
head t = case uncons t of { Nothing -> errorEmptyText "head"; Just (c, _) -> c }

tail :: Text -> Text
{-# INLINABLE tail #-}
tail t = case uncons t of { Nothing -> empty; Just (_, t) -> t }

last :: Text -> Char
{-# INLINABLE last #-}
last t = case unsnoc t of { Nothing -> errorEmptyText "last"; Just (_, c) -> c }

init :: Text -> Text
{-# INLINABLE init #-}
init t = case unsnoc t of { Nothing -> empty; Just (t, _) -> t }

null :: Text -> Bool
{-# INLINABLE null #-}
null (Text bs) = V.null bs

length :: Text -> Int
{-# INLINABLE length #-}
length (Text (V.PrimVector ba s l)) = go s
  where
    !sl = s + l
    go !i | i >= sl = 0
          | otherwise = let (_, j) = decodeChar ba i in 1 + go (i+j)

--------------------------------------------------------------------------------
-- * Transformations
--
-- | /O(n)/ 'map' @f@ @t@ is the 'Text' obtained by applying @f@ to
-- each element of @t@. Performs replacement on invalid scalar values.
--
map :: (Char -> Char) -> Text -> Text
map f = \ t@(Text v) -> packN (V.length v + 3) (List.map f (unpack t)) -- the 3 bytes buffer is here for optimizing ascii mapping
{-# INLINE map #-}                                                     -- because we do resize if less than 3 bytes left when building

{-
-- | /O(n)/ The 'intercalate' function takes a 'Text' and a list of
-- 'Text's and concatenates the list after interspersing the first
-- argument between each element of the list.
intercalate :: Text -> [Text] -> Text
intercalate t = concat . (F.intersperse t)
{-# INLINE intercalate #-}

-- | /O(n)/ The 'intersperse' function takes a character and places it
-- between the characters of a 'Text'.  Subject to fusion.  Performs
-- replacement on invalid scalar values.
intersperse     :: Char -> Text -> Text
intersperse c t = unstream (S.intersperse (safe c) (stream t))
{-# INLINE intersperse #-}

-- | /O(n)/ Reverse the characters of a string. Subject to fusion.
reverse :: Text -> Text
reverse t = S.reverse (stream t)
{-# INLINE reverse #-}

-}

--------------------------------------------------------------------------------

errorEmptyText :: String -> a
{-# NOINLINE errorEmptyText #-}
errorEmptyText fun = error ("Data.Text." ++ fun ++ ": empty Text")

--------------------------------------------------------------------------------
