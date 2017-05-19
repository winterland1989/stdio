{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface, CApiFFI #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GHCForeignImportPrim #-}

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
  , create
  , createN
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
  , transpose
  -- ** Special folds
  , concat
  , concatMap
  , any
  , all
  , maximum
  , minimum
  ) where

import GHC.Prim
import GHC.Types
import GHC.ST
import Data.Primitive.ByteArray
import qualified Data.List as List

#if MIN_VERSION_base(4,9,0)
import Data.Semigroup (Semigroup((<>)))
#endif
import Data.Monoid (Monoid(..))

import Data.Data
import Data.Bits (shiftL)
import GHC.Word
import Control.DeepSeq
import Control.Exception (throwIO, assert)
import Control.Monad
import Control.Monad.ST
import Control.Monad.ST.Unsafe
import Debug.Trace (traceShow)

import Foreign.C.Types

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
import System.IO.Unsafe


--------------------------------------------------------------------------------
-- | 'Bytes' is just primitive word8 vectors.
--
--
data Bytes = Bytes {-# UNPACK #-} !ByteArray -- payload
                   {-# UNPACK #-} !Int       -- offset
                   {-# UNPACK #-} !Int       -- length

instance Eq Bytes where
    (==) = equateBytes
    {-# INLINABLE (==) #-}

equateBytes :: Bytes -> Bytes -> Bool
equateBytes (Bytes (ByteArray baA#) sA lA) (Bytes (ByteArray baB#) sB lB) =
    let r = unsafeDupablePerformIO $
            c_memcmp baA# (fromIntegral sA)
                     baB# (fromIntegral sB) (fromIntegral $ min lA lB)
    in lA == lB && r == 0

instance Ord Bytes where
    compare = compareBytes
    {-# INLINABLE compare #-}

compareBytes :: Bytes -> Bytes -> Ordering
compareBytes (Bytes (ByteArray baA#) sA lA) (Bytes (ByteArray baB#) sB lB) =
    let r = unsafeDupablePerformIO $
            c_memcmp baA# (fromIntegral sA)
                     baB# (fromIntegral sB) (fromIntegral $ min lA lB)
    in case r `compare` 0 of
        EQ  -> lA `compare` lB
        x  -> x

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

instance NFData Bytes where
    rnf Bytes{} = ()

instance Show Bytes where
    showsPrec p ps r = showsPrec p (List.map w2c $ unpack ps) r

instance Read Bytes where
    readsPrec p str = [ (packN 16 (List.map c2w x), y) | (x, y) <- readsPrec p str ]

instance Data Bytes where
    gfoldl f z txt = z pack `f` unpack txt
    toConstr _     = error "Data.Bytes.toConstr"
    gunfold _ _    = error "Data.Bytes.gunfold"
    dataTypeOf _   = mkNoRepType "Data.Bytes"

--------------------------------------------------------------------------------
-- Creating 'Bytes' and conversion between list
--
-- | Create a 'Bytes'.
--
create :: Int  -- length's upper bound
       -> (forall s. MutableByteArray s -> ST s ())  -- initialization function
       -> Bytes
create l fill = runST (do
        mba <- newByteArray l
        fill mba
        ba <- unsafeFreezeByteArray mba
        return (Bytes ba 0 l)
    )
{-# INLINE create #-}

-- | Create a 'Bytes' up to a specific length.
--
createN :: Int  -- length's upper bound
       -> (forall s. MutableByteArray s -> ST s Int)  -- initialization function which return the actual length
                                                      -- (must be smaller than upper bound)
       -> Bytes
createN l fill = runST (do
        mba <- newByteArray l
        l' <- fill mba
        ba <- unsafeFreezeByteArray mba
        assert (l' <= l) $ return (Bytes ba 0 l')
    )
{-# INLINE createN #-}

-- | /O(1)/. The empty 'Bytes'.
--
empty :: Bytes
empty = create 0 (\_ -> return ())

-- | /O(1)/ Convert a 'Word8' into a 'Bytes'
singleton :: Word8 -> Bytes
singleton c = create 1 (\ mba -> writeByteArray mba 0 c)
{-# INLINE [1] singleton #-} -- Inline [1] for intercalate rule

-- | /O(n)/ Convert a list into a 'Bytes'
--
-- NOTE. This function have to force the entire list to found out the length for allocation.
-- Most of the time you should use 'packN' instead.
--
pack :: [Word8] -> Bytes
pack ws0 = create (List.length ws0) (go 0 ws0)
  where
    go !i []     !_   = return ()
    go !i (w:ws) !mba = writeByteArray mba i w >> go (i+1) ws mba

-- | /O(n)/ Convert a list into a 'Bytes' with an approximate size.
--
-- If the list's length is large than the size given, we simply double the buffer size
-- and continue building(using 'resizeMutableByteArray#' prim).
-- This function consume list lazily, and run faster than 'pack'
-- (even the initial size is many times smaller).
--

packN :: Int -> [Word8] -> Bytes
packN n0 ws0 = runST (do mba <- newByteArray n0
                         SP3 i _ mba' <- foldM go (SP3 0 n0 mba) ws0
                         ba <- unsafeFreezeByteArray mba'
                         return (Bytes ba 0 i)
                     )
  where
    go (SP3 i n mba) x = if i < n then do writeByteArray mba i x
                                          return (SP3 (i+1) n mba)
                                  else do let !n' = n*2
                                          mba' <- resizeMutableByteArray mba n'
                                          writeByteArray mba' i x
                                          return (SP3 (i+1) n' mba')
-- | helper type
data SP3 s = SP3 {-# UNPACK #-}!Int
                {-# UNPACK #-}!Int
                {-# UNPACK #-}!(MutableByteArray s)
{-# INLINE packN #-}


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
append (Bytes _ _  0)  b            = b
append a              (Bytes _ _ 0) = a
append (Bytes baA sA lA) (Bytes baB sB lB) = create (lA+lB) $ \ mba -> do
    copyByteArray mba 0  baA sA lA
    copyByteArray mba lA baB sB lB
{-# INLINE append #-}

-- | /O(n)/ 'cons' is analogous to (:) for lists, but of different
-- complexity, as it requires making a copy.
cons :: Word8 -> Bytes -> Bytes
cons w (Bytes ba s l) = create (l+1) $ \ mba -> do
    writeByteArray mba 0 w
    copyByteArray mba 1 ba s l
{-# INLINE cons #-}

-- | /O(n)/ Append a byte to the end of a 'Bytes'
snoc :: Bytes -> Word8 -> Bytes
snoc (Bytes ba s l) w = create (l+1) $ \ mba -> do
    copyByteArray mba 0 ba s l
    writeByteArray mba l w
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
map f = \ (Bytes ba s l) -> create l (go ba (l+s) s)
  where
    go ba !sl !i !mba  | i >= sl = return ()
                       | otherwise = do let !x = indexByteArray ba i
                                        writeByteArray mba i (f x)
                                        go ba sl (i+1) mba
{-# INLINE map #-}

-- | /O(n)/ 'reverse' @xs@ efficiently returns the elements of @xs@ in reverse order.
reverse :: Bytes -> Bytes
reverse (Bytes (ByteArray ba#) s l) =
    create l (\ (MutableByteArray mba#) ->
        unsafeIOToST (c_reverse mba# ba# (fromIntegral (s + l))))
{-# INLINE reverse #-}

-- | /O(n)/ The 'intersperse' function takes a 'Word8' and a
-- 'Bytes' and \`intersperses\' that byte between the elements of
-- the 'Bytes'.  It is analogous to the intersperse function on
-- Lists.
intersperse :: Word8 -> Bytes -> Bytes
intersperse w bs@(Bytes (ByteArray ba#) s l)
    | l < 2  = bs
    | otherwise = create (2*l-1) (\ (MutableByteArray mba#) ->
            unsafeIOToST (c_intersperse mba# ba# (fromIntegral s) (fromIntegral l) (fromIntegral w))
        )
{-# INLINE intersperse #-}

-- | /O(n)/ The 'intercalate' function takes a 'Bytes' and a list of
-- 'Bytes's and concatenates the list after interspersing the first
-- argument between each element of the list.
intercalate :: Bytes -> [Bytes] -> Bytes
intercalate s = concat . List.intersperse s
{-# INLINE [1] intercalate #-}

-- | /O(n)/ intercalateByte. An efficient way to join [Bytes]
-- with a byte. It's faster than @intercalate (singleton c)@.
--
intercalateByte :: Word8 -> [Bytes] -> Bytes
intercalateByte w = \ bss -> create (len bss) (copy 0 bss)
  where
    len []                = 0
    len [Bytes _ _ l]     = l
    len (Bytes _ _ l:bss) = l + 1 + len bss

    copy !i []                 !mba = return ()
    copy !i (Bytes ba s l:bss) !mba = do let i' = i + l
                                         copyByteArray mba i' ba s l
                                         copy i' bss mba
{-# INLINE intercalateByte #-}

-- | The 'transpose' function transposes the rows and columns of its
-- 'Bytes' argument.
--
transpose :: [Bytes] -> [Bytes]
transpose bss =
    List.map (packN (List.length bss)) . List.transpose . List.map unpack $ bss
{-# INLINE transpose #-}

--------------------------------------------------------------------------------
--
-- Special folds
--
-- | /O(n)/ Concatenate a list of ByteStrings.
--
-- Note: 'concat' have to force the entire list to filter out empty 'Bytes' and calculate
-- the length for allocation.
--
concat :: [Bytes] -> Bytes
concat bss = case pre bss 0 [] of ([], _)    -> empty
                                  ([bs], _)  -> bs
                                  (bss', l') -> create l' (copy bss' l')
  where
    -- pre scan to filter empty bytes and calculate total length
    pre :: [Bytes] -> Int -> [Bytes] -> ([Bytes], Int)
    pre !bacc !lacc [] = (bacc, lacc)
    pre !bacc !lacc (bs@(Bytes _ _ l):bss)
        | l <= 0    = pre bacc lacc bss
        | otherwise = pre (bs:bacc) (l+lacc) bss

    copy [] !_ !mba       = return ()
    copy (bs:bss) !i !mba = do let Bytes ba s l = bs
                                   i' = i - l
                               copyByteArray mba i' ba s l
                               copy bss i' mba

-- | Map a function over a 'Bytes' and concatenate the results
concatMap :: (Word8 -> Bytes) -> Bytes -> Bytes
concatMap = undefined

-- | /O(n)/ Applied to a predicate and a Bytes, 'any' determines if
-- any element of the 'Bytes' satisfies the predicate.
any :: (Word8 -> Bool) -> Bytes -> Bool
any = undefined

-- | /O(n)/ Applied to a predicate and a 'Bytes', 'all' determines
-- if all elements of the 'Bytes' satisfy the predicate.
all :: (Word8 -> Bool) -> Bytes -> Bool
all = undefined

-- | /O(n)/ 'maximum' returns the maximum value from a 'Bytes'
-- This function will fuse.
-- An exception will be thrown in the case of an empty Bytes.
maximum :: Bytes -> Word8
maximum = undefined

-- | /O(n)/ 'minimum' returns the minimum value from a 'Bytes'
-- This function will fuse.
-- An exception will be thrown in the case of an empty Bytes.
minimum :: Bytes -> Word8
minimum = undefined

--------------------------------------------------------------------------------
--
-- Unsafe operations
--

unsafeIndex :: Bytes -> Int -> Word8
unsafeIndex (Bytes ba s _) idx = indexByteArray ba (idx + s)
{-# INLINE unsafeIndex #-}

--------------------------------------------------------------------------------

resizeMutableByteArray :: MutableByteArray s -> Int -> ST s (MutableByteArray s)
resizeMutableByteArray (MutableByteArray mba#) (I# i#) =
    ST (\ s# ->
            let (# s'#, mba'# #) = resizeMutableByteArray# mba# i# s#
            in (# s'#, MutableByteArray mba'# #)
       )
{-# INLINE resizeMutableByteArray #-}


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

foreign import ccall unsafe "bytes.c intersperse"
    c_intersperse :: MutableByteArray# s -> ByteArray# -> CSize -> CSize -> CChar -> IO ()

foreign import ccall unsafe "bytes.c _memcmp"
    c_memcmp :: ByteArray# -> CSize -> ByteArray# -> CSize -> CSize -> IO CInt
