{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE RankNTypes #-}

-- |
--
--
--
--
--

module Data.Bytes
  ( -- * The 'Bytes' type
    Bytes(..)
    -- * Creating 'Bytes' and conversion between list
  , empty
  , singleton
  , pack, packN
  , unpack
    -- * Basic interface
  , append
  , null
  , length
  , cons, snoc, uncons, unsnoc
  , head, tail
  , last, init
  -- * Transforming Bytes
  , map
  , reverse
  , intersperse
  , intercalate
  ) where

import GHC.Prim
import GHC.Types
import Data.Primitive.ByteArray
import qualified Data.List as List

#if MIN_VERSION_base(4,9,0)
import Data.Semigroup (Semigroup((<>)))
#endif
import Data.Monoid (Monoid(..))

import Data.Data
import GHC.Word
import Control.DeepSeq
import Control.Exception (throwIO)
import Control.Monad
import Control.Monad.ST
import Control.Monad.ST.Unsafe
import Debug.Trace (traceShow)

import Foreign.C.Types (CInt(..), CSize(..))

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



--------------------------------------------------------------------------------
-- | 'Bytes' is just primitive word8 vectors.
--
--
data Bytes = Bytes {-# UNPACK #-} !ByteArray -- payload
                   {-# UNPACK #-} !Int       -- s
                   {-# UNPACK #-} !Int       -- length

instance Eq Bytes where
    (==)    = equateBytes

equateBytes :: Bytes -> Bytes -> Bool
equateBytes = undefined

instance Ord Bytes where
    compare = compareBytes

compareBytes :: Bytes -> Bytes -> Ordering
compareBytes = undefined

#if MIN_VERSION_base(4,9,0)
instance Semigroup Bytes where
    (<>)    = append
#endif

instance Monoid Bytes where
    mempty  = empty
#if MIN_VERSION_base(4,9,0)
    mappend = (<>)
#else
    mappend = append
#endif
    mconcat = concat


concat :: [Bytes] -> Bytes
concat = undefined

instance NFData Bytes where
    rnf Bytes{} = ()

instance Show Bytes where
    showsPrec p ps r = showsPrec p (unpack ps) r

instance Read Bytes where
    readsPrec p str = [ (pack x, y) | (x, y) <- readsPrec p str ]

instance Data Bytes where
    gfoldl f z txt = z pack `f` unpack txt
    toConstr _     = error "Data.Bytes.toConstr"
    gunfold _ _    = error "Data.Bytes.gunfold"
    dataTypeOf _   = mkNoRepType "Data.Bytes"

--------------------------------------------------------------------------------
-- Creating 'Bytes' and conversion between list
--
-- | Create a 'Bytes'
--
create :: Int  -- length's upper bound
       -> (forall s. MutableByteArray s -> ST s Int)  -- initialization function which return the actual length
                                                      -- (must be smaller than upper bound)
       -> Bytes
create l fill = runST (do
        mba <- newByteArray l
        l' <- fill mba
        ba <- unsafeFreezeByteArray mba
        return (Bytes ba 0 l')
    )
{-# INLINE create #-}

-- | /O(1)/. The empty 'Bytes'.
--
empty :: Bytes
empty = create 0 (\_ -> return 0)

-- | /O(1)/ Convert a 'Word8' into a 'Bytes'
singleton :: Word8 -> Bytes
singleton c = create 1 (\ mba -> writeByteArray mba 0 c >> return 1)

-- | /O(n)/ Convert a list into a 'Bytes'
--
-- This function have to force the entire list to found out the length for allocation.
-- If you know the length's upper bound, please use 'packN'.
--
pack :: [Word8] -> Bytes
pack ws0 = create (List.length ws0) (\ mba -> go mba 0 ws0)
  where
    go :: MutableByteArray s -> Int -> [Word8] -> ST s Int
    go !_   !i []     = return i
    go !mba !i (w:ws) = writeByteArray mba i w >> go mba (i+1) ws

-- | /O(n)/ Convert a list into a 'Bytes' with given length's upper bound.
--
-- If the list's length is large than given bound, the rest words are discarded.
--
packN :: Int -> [Word8] -> Bytes
packN l ws0 = create l (\ mba -> go mba 0 ws0)
  where
    go :: MutableByteArray s -> Int -> [Word8] -> ST s Int
    go !_   !i []     = return i
    go !mba !i (w:ws) | i <= l  = writeByteArray mba i w >> go mba (i+1) ws
                      | otherwise = return i

-- | /O(n)/ Convert 'Bytes' to a 'Word8' list.
--
unpack :: Bytes -> [Word8]
unpack (Bytes ba s l) = List.map (indexByteArray ba) [s..s+l-1]
{-# INLINE unpack #-}

--------------------------------------------------------------------------------
-- Basic interface
--
-- |  /O(1)/ The length of a 'Bytes'.
--
length :: Bytes -> Int
length (Bytes _ _ l) = l
{-# INLINE length #-}

-- | /O(1)/ Test whether a 'Bytes' is empty.
null :: Bytes -> Bool
null bs = length bs == 0
{-# INLINE null #-}

-- | /O()/
append :: Bytes -> Bytes -> Bytes
append (Bytes ba0 offset0 len0) (Bytes ba1 offset1 len1) = create l' $ \ mba -> do
    copyByteArray mba 0    ba0 offset0 len0
    copyByteArray mba len0 ba1 offset1 len1
    return l'
  where l' = len0 + len1
{-# INLINE append #-}

-- | /O(n)/ 'cons' is analogous to (:) for lists, but of different
-- complexity, as it requires making a copy.
cons :: Word8 -> Bytes -> Bytes
cons w (Bytes ba s l) = create l' $ \ mba -> do
    writeByteArray mba 0 w
    copyByteArray mba 1 ba s l
    return l'
  where l' = l + 1
{-# INLINE cons #-}

-- | /O(n)/ Append a byte to the end of a 'Bytes'
snoc :: Bytes -> Word8 -> Bytes
snoc (Bytes ba s l) w = create l' $ \ mba -> do
    copyByteArray mba 0 ba s l
    writeByteArray mba l w
    return l'
  where l' = l + 1
{-# INLINE snoc #-}

-- | /O(1)/ Extract the head and tail of a Bytes, returning Nothing
-- if it is empty.
--
uncons :: Bytes -> Maybe (Word8, Bytes)
uncons (Bytes ba s l)
    | l <= 0    = Nothing
    | otherwise = Just (indexByteArray ba s, Bytes ba (s+1) (l-1))
{-# INLINE uncons #-}

-- | /O(1)/ Extract the 'init' and 'last' of a Bytes, returning Nothing
-- if it is empty.
--
unsnoc :: Bytes -> Maybe (Bytes, Word8)
unsnoc (Bytes ba s l)
    | l <= 0    = Nothing
    | otherwise = Just (Bytes ba s (l-1), indexByteArray ba (s+l-1))
{-# INLINE unsnoc #-}

-- | /O(1)/ Extract the first element of a Bytes, which must be non-empty.
-- An exception will be thrown in the case of an empty Bytes.
--
head :: Bytes -> Word8
head (Bytes ba s l)
    | l <= 0    = errorEmptyBytes "head"
    | otherwise = indexByteArray ba s
{-# INLINE head #-}

-- | /O(1)/ Extract the elements after the head of a Bytes, which must be non-empty.
-- An exception will be thrown in the case of an empty Bytes.
tail :: Bytes -> Bytes
tail (Bytes ba s l)
    | l <= 0    = errorEmptyBytes "tail"
    | otherwise = Bytes ba (s+1) (l-1)
{-# INLINE tail #-}

-- | /O(1)/ Extract the first element of a Bytes, which must be non-empty.
-- An exception will be thrown in the case of an empty Bytes.
--
last :: Bytes -> Word8
last (Bytes ba s l)
    | l <= 0    = errorEmptyBytes "last"
    | otherwise = indexByteArray ba (s+l+1)
{-# INLINE last #-}

-- | /O(1)/ Extract the elements after the head of a Bytes, which must be non-empty.
-- An exception will be thrown in the case of an empty Bytes.
init :: Bytes -> Bytes
init (Bytes ba s l)
    | l <= 0    = errorEmptyBytes "init"
    | otherwise = Bytes ba s (l-1)
{-# INLINE init #-}

--------------------------------------------------------------------------------
-- * Transforming Bytes
--
-- | /O(n)/ 'map' @f xs@ is the Bytes obtained by applying @f@ to each
-- element of @xs@.
map :: (Word8 -> Word8) -> Bytes -> Bytes
map f (Bytes ba s l) = create l (go 0)
  where
    go !i !mba  | i >= l = return l
                | otherwise = do let !x = indexByteArray ba (s+i)
                                 writeByteArray mba i (f x)
                                 go (i+1) mba
{-# INLINE map #-}

-- | /O(n)/ 'reverse' @xs@ efficiently returns the elements of @xs@ in reverse order.
reverse :: Bytes -> Bytes
reverse (Bytes (ByteArray ba#) s l) =
    create l (\ (MutableByteArray mba#) ->
        unsafeIOToST (c_reverse mba# ba# (fromIntegral (s + l))) >> return l)
{-# INLINE reverse #-}

-- | /O(n)/ The 'intersperse' function takes a 'Word8' and a
-- 'Bytes' and \`intersperses\' that byte between the elements of
-- the 'Bytes'.  It is analogous to the intersperse function on
-- Lists.
intersperse :: Word8 -> Bytes -> Bytes
intersperse w bs@(Bytes ba s l) | l < 2  = bs
                                | otherwise = create (2*l-1) $ \ mba -> go mba 0
  where
    go !mba !i | i >= l = return l
               | otherwise = do let !x = indexByteArray ba (s+i) :: Word8
                                writeByteArray mba (s+i*2) x
                                writeByteArray mba (s+i*2+1) w
                                go mba (i+1)
{-# INLINE intersperse #-}

intercalate = undefined

--------------------------------------------------------------------------------
--
-- Unsafe operations
--

unsafeIndex :: Bytes -> Int -> Word8
unsafeIndex (Bytes ba s _) idx = indexByteArray ba (idx + s)
{-# INLINE unsafeIndex #-}

--------------------------------------------------------------------------------

-- | Conversion between 'Word8' and 'Char'. Should compile to a no-op.
--
w2c :: Word8 -> Char
w2c (W8# w#) = C# (chr# (word2Int# w#))
{-# INLINE w2c #-}

-- | Unsafe conversion between 'Char' and 'Word8'. This is a no-op and
-- silently truncates to 8 bits Chars > '\255'. It is provided as
-- convenience for Bytes construction.
c2w :: Char -> Word8
c2w (C# c#) = W8# (int2Word# (ord# c#))
{-# INLINE c2w #-}

-- Common up near identical calls to `error' to reduce the number
-- constant strings created when compiled:
errorEmptyBytes :: String -> a
errorEmptyBytes fun = moduleError fun "empty Bytes"
{-# NOINLINE errorEmptyBytes #-}

moduleError :: String -> String -> a
moduleError fun msg = error (moduleErrorMsg fun msg)
{-# NOINLINE moduleError #-}

moduleErrorIO :: String -> String -> IO a
moduleErrorIO fun msg = throwIO . userError $ moduleErrorMsg fun msg
{-# NOINLINE moduleErrorIO #-}

moduleErrorMsg :: String -> String -> String
moduleErrorMsg fun msg = "Data.Bytes." ++ fun ++ ':':' ':msg

--------------------------------------------------------------------------------
-- FFI
--

foreign import ccall unsafe "bytes.c reverse"
    c_reverse :: MutableByteArray# s -> ByteArray# -> CSize -> IO ()

foreign import ccall unsafe "string.h memcmp"
    c_memcmp :: ByteArray# -> CSize -> ByteArray# -> CSize -> CSize -> IO CInt
