{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

#include "MachDeps.h"

module Data.Vector (
  -- * Vec typeclass
    Vec(..)
  -- * Boxed and unboxed vector type
  , Vector(..)
  , pattern VecPat
  , PrimVector(..)
  , pvW16
  -- ** 'Word8' vector
  , Bytes
  , pattern BytesPat
  , bAsc
  , w2c, c2w
  -- * Basic creating
  , create, creating, createN
  , empty, singleton
  -- * Conversion between list
  , pack, packN, packR, packRN
  , unpack, unpackR
  -- * Basic interface
  , append
  , null
  , length
  , cons, snoc, uncons, unsnoc
  , head, tail
  , last, init
  -- * Transforming primitive vector
  , map
  , reverse
  , intersperse
  , intercalate
  , intercalateElem
  , transpose
  -- * Reducing primitive vector (folds)
  , foldl
  , foldl'
  , foldl1'
  , foldr
  , foldr'
  , foldr1'
    -- ** Special folds
  , concat
  , concatMap
  , any
  , all
  , maximum
  , minimum
  -- * Building primitive vector
  -- ** Scans
  , scanl
  , scanl1
  , scanr
  , scanr1
  -- ** Accumulating maps
  , mapAccumL
  , mapAccumR
  -- ** Generating and unfolding primitive vector
  , replicate
  , unfoldr
  , unfoldrN
  -- * Substrings
  , take
  , drop
  , slice
  , splitAt


  -- * Misc
  , defaultInitSize
  , chunkOverhead
  , defaultChunkSize
  , smallChunkSize
 ) where

import Control.DeepSeq
import Control.Exception (assert)
import GHC.Exts (IsList(..), IsString(..))
import Control.Monad.ST.Unsafe
import Control.Monad.ST
import Control.Monad
import Data.Primitive
import Data.Primitive.Types
import Data.Primitive.ByteArray
import Data.Primitive.SmallArray
import Data.Primitive.PrimArray
import Data.Array
import GHC.Word
import GHC.Prim
import GHC.Ptr (Ptr(..))
import GHC.CString
import Data.Typeable
import Data.Data
import Data.Bits
import qualified Data.List as List
import Data.Foldable (foldlM, foldrM)
import GHC.Types
import Foreign.C.Types
import Foreign.Storable (peekElemOff)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as Q
import Data.LiteralQ

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


#if MIN_VERSION_base(4,9,0)
import Data.Semigroup (Semigroup((<>)))
#endif
import Data.Monoid (Monoid(..))

-- | Typeclass for box and unboxed vectors, which are created by slicing arrays.
--
class (Arr (MArray v) (IArray v) a) => Vec v a where
    -- | Vector's mutable array type
    type MArray v :: * -> * -> *
    -- | Vector's immutable array type
    type IArray v :: * -> *
    -- | Get underline array and slice range(offset and length).
    toArr :: v a -> (IArray v a, Int, Int)
    -- | Create a vector by slicing an array(with offset and length).
    fromArr :: IArray v a -> Int -> Int -> v a


--------------------------------------------------------------------------------
-- | Boxed vector
--
data Vector a = Vector
    {-# UNPACK #-} !(SmallArray a) -- payload
    {-# UNPACK #-} !Int         -- offset in elements of type a rather than in bytes
    {-# UNPACK #-} !Int         -- length in elements of type a rather than in bytes
    deriving (Typeable, Data)

instance Vec Vector a where
    type MArray Vector = SmallMutableArray
    type IArray Vector = SmallArray
    toArr (Vector arr s l) = (arr, s, l)
    {-# INLINE toArr #-}
    fromArr arr s l = Vector arr s l
    {-# INLINE fromArr #-}

-- | A pattern synonyms for matching the underline array, offset and length.
--
pattern VecPat ba s l <- (toArr -> (ba,s,l))

instance Eq a => Eq (Vector a) where
    v1 == v2 = eqVector v1 v2
    {-# INLINABLE (==) #-}

eqVector :: Eq a => Vector a -> Vector a -> Bool
eqVector (Vector baA sA lA) (Vector baB sB lB) = lA == lB && go sA sB
  where
    !slA = sA + lA
    go !i !j
        | i >= slA  = True
        | otherwise =
            (indexSmallArray baA i == indexSmallArray baB j) && go (i+1) (j+1)

instance {-# OVERLAPPABLE #-} Ord a => Ord (Vector a) where
    compare = compareVector
    {-# INLINABLE compare #-}

compareVector :: Ord a => Vector a -> Vector a -> Ordering
compareVector (Vector baA sA lA) (Vector baB sB lB) = go sA sB
  where
    !slA = sA + lA
    !slB = sB + lB
    go !i !j | i >= slA  = slB `compare` j
             | j >= slB  = slA `compare` i
             | otherwise = let o = indexSmallArray baA i `compare` indexSmallArray baB j
                           in case o of EQ -> go (i+1) (j+1)
                                        x  -> x

#if MIN_VERSION_base(4,9,0)
instance Prim a => Semigroup (Vector a) where
    (<>)    = append
#endif

instance Prim a => Monoid (Vector a) where
    mempty  = empty
#if MIN_VERSION_base(4,9,0)
    mappend = (<>)
#else
    mappend = append
#endif
    mconcat = concat

instance NFData (Vector a) where
    rnf Vector{} = ()

instance (Show a) => Show (Vector a) where
    showsPrec p v = showsPrec p (unpack v)

instance (Read a) => Read (Vector a) where
    readsPrec p str = [ (pack x, y) | (x, y) <- readsPrec p str ]

--------------------------------------------------------------------------------
-- | Primitive vector
--
data PrimVector a = PrimVector
    {-# UNPACK #-} !(PrimArray a) -- payload
    {-# UNPACK #-} !Int         -- offset in elements of type a rather than in bytes
    {-# UNPACK #-} !Int         -- length in elements of type a rather than in bytes
  deriving (Typeable, Data)

pvW16 :: Q.QuasiQuoter
pvW16 = Q.QuasiQuoter
    (word16LiteralLE $ \ len addr -> [| word16VectorFromAddr len $(addr) |])
    (error "Cannot use bAsc as a pattern")
    (error "Cannot use bAsc as a type")
    (error "Cannot use bAsc as a dec")

word16VectorFromAddr :: Int -> Addr# -> PrimVector Word16
word16VectorFromAddr l addr# = unsafeDupablePerformIO $ do
    mba <- newArr l
    go l (Ptr addr#) mba 0
    ba <- unsafeFreezePrimArray mba :: IO (PrimArray Word16)
    return (PrimVector ba 0 l)
  where
    go l ptr mba idx = do
#ifdef WORDS_BIGENDIAN
        when (idx < l) $ do
            w1 <- peekElemOff ptr (idx*2) :: IO Word8
            w2 <- peekElemOff ptr (idx*2+1) :: IO Word8
            writePrimArray mba idx (fromIntegral w2 `shiftL` 8 .|. fromIntegral w1 :: Word16)
            go l ptr mba (idx+1)
#else
        copyMutablePrimArrayFromPtr mba 0 ptr l
#endif
{-# NOINLINE word16VectorFromAddr #-} -- don't dump every literal with this code

instance Prim a => Vec PrimVector a where
    type MArray PrimVector = MutablePrimArray
    type IArray PrimVector = PrimArray
    toArr (PrimVector arr s l) = (arr, s, l)
    {-# INLINE toArr #-}
    fromArr arr s l = PrimVector arr s l
    {-# INLINE fromArr #-}

instance {-# OVERLAPPABLE #-} Prim a => Eq (PrimVector a) where
    (==) = equateBytes
    {-# INLINABLE (==) #-}

equateBytes :: forall a. Prim a => PrimVector a -> PrimVector a -> Bool
equateBytes (PrimVector (PrimArray (ByteArray baA#)) sA lA)
            (PrimVector (PrimArray (ByteArray baB#)) sB lB) =
    let r = unsafeDupablePerformIO $
            c_memcmp baA# (fromIntegral $ sA * siz)
                     baB# (fromIntegral $ sB * siz) (fromIntegral $ min lA lB * siz)
    in lA == lB && r == 0
  where siz = sizeOf (undefined :: a)

instance {-# OVERLAPPABLE #-} (Prim a, Ord a) => Ord (PrimVector a) where
    compare = comparePrimVector
    {-# INLINABLE compare #-}

instance {-# OVERLAPPING #-} Ord (PrimVector Word8) where
    compare = compareBytes
    {-# INLINABLE compare #-}

comparePrimVector :: (Prim a, Ord a) => PrimVector a -> PrimVector a -> Ordering
comparePrimVector (PrimVector baA sA lA) (PrimVector baB sB lB) = go sA sB
  where
    !slA = sA + lA
    !slB = sB + lB
    go !i !j | i >= slA  = slB `compare` j
             | j >= slB  = slA `compare` i
             | otherwise = let o = indexPrimArray baA i `compare` indexPrimArray baB j
                           in case o of EQ -> go (i+1) (j+1)
                                        x  -> x

compareBytes :: PrimVector Word8 -> PrimVector Word8 -> Ordering
compareBytes (PrimVector (PrimArray (ByteArray baA#)) sA lA)
             (PrimVector (PrimArray (ByteArray baB#)) sB lB) =
    let r = unsafeDupablePerformIO $
            c_memcmp baA# (fromIntegral sA)
                     baB# (fromIntegral sB) (fromIntegral $ min lA lB)
    in case r `compare` 0 of
        EQ  -> lA `compare` lB
        x  -> x

#if MIN_VERSION_base(4,9,0)
instance Prim a => Semigroup (PrimVector a) where
    (<>)    = append
#endif

instance Prim a => Monoid (PrimVector a) where
    mempty  = empty
#if MIN_VERSION_base(4,9,0)
    mappend = (<>)
#else
    mappend = append
#endif
    mconcat = concat

instance NFData (PrimVector a) where
    rnf PrimVector{} = ()

instance (Prim a, Show a) => Show (PrimVector a) where
    showsPrec p v = showsPrec p (unpack v)

instance (Prim a, Read a) => Read (PrimVector a) where
    readsPrec p str = [ (pack x, y) | (x, y) <- readsPrec p str ]

--------------------------------------------------------------------------------

-- | 'Bytes' is just primitive word8 vectors.
type Bytes = PrimVector Word8

-- | Conversion between 'Word8' and 'Char'. Should compile to a no-op.
--
w2c :: Word8 -> Char
w2c (W8# w#) = C# (chr# (word2Int# w#))
{-# INLINE w2c #-}

-- | Unsafe conversion between 'Char' and 'Word8'. This is a no-op and
-- silently truncates to 8 bits Chars > '\255'. It is provided as
-- convenience for PrimVector construction.
c2w :: Char -> Word8
c2w (C# c#) = W8# (int2Word# (ord# c#))
{-# INLINE c2w #-}

bAsc :: Q.QuasiQuoter
bAsc = Q.QuasiQuoter
    (asciiLiteral $ \ len addr -> [| bytesFromAddr len $(addr) |])
    (error "Cannot use bAsc as a pattern")
    (error "Cannot use bAsc as a type")
    (error "Cannot use bAsc as a dec")

bytesFromAddr :: Int -> Addr# -> Bytes
bytesFromAddr l addr# = unsafeDupablePerformIO $ do
    mba <- newArr l
    copyMutablePrimArrayFromPtr mba 0 (Ptr addr#) l
    ba <- unsafeFreezePrimArray mba
    return (PrimVector ba 0 l)
{-# NOINLINE bytesFromAddr #-} -- don't dump every literal with this code

pattern BytesPat ba s l <- (toArr -> (PrimArray ba,s,l))

--------------------------------------------------------------------------------
-- Basic creating

-- | Create a vector.
create :: Vec v a
       => Int  -- length in elements of type @a@
       -> (forall s. MArray v s a -> ST s ())  -- initialization function
       -> v a
create l fill = runST (do
        mba <- newArr l
        fill mba
        ba <- unsafeFreezeArr mba
        return $! fromArr ba 0 l
    )
{-# INLINE create #-}

-- | Create a vector, return both the vector and the monadic result during creating.
--
creating :: Vec v a
         => Int  -- length in elements of type @a@
         -> (forall s. MArray v s a -> ST s b)  -- initialization function
         -> (b, v a)
creating l fill = runST (do
        mba <- newArr l
        b <- fill mba
        ba <- unsafeFreezeArr mba
        let !v = fromArr ba 0 l
        return (b, v)
    )
{-# INLINE creating #-}

-- | Create a vector up to a specific length.
--
createN :: Vec v a
        => Int  -- length's upper bound
        -> (forall s. MArray v s a -> ST s Int)  -- initialization function which return the actual length
                                                         -- (must be smaller than upper bound)
        -> v a
createN l fill = runST (do
        mba <- newArr l
        l' <- fill mba
        ba <- unsafeFreezeArr mba
        assert (l' <= l) (return $! fromArr ba 0 l')
    )
{-# INLINE createN #-}

-- | /O(1)/. The empty vector.
--
empty :: Vec v a => v a
empty = create 0 (\_ -> return ())
{-# INLINE empty #-}

-- | /O(1)/. Single element vector..
singleton :: Vec v a => a -> v a
singleton c = create 1 (\ mba -> writeArr mba 0 c)
{-# INLINE singleton #-}

--------------------------------------------------------------------------------
-- Conversion between list
--
-- | /O(n)/ Convert a list into a vector
--
-- Alias for @'packN' 'defaultInitSize'@.
--
pack :: Vec v a => [a] -> v a
pack = packN defaultInitSize
{-# INLINE pack #-}

-- | The chunk size used for I\/O. Currently set to 32k, less the memory management overhead
defaultChunkSize :: Int
defaultChunkSize = 32 * 1024 - chunkOverhead
{-# INLINE defaultChunkSize #-}

-- | The recommended chunk size. Currently set to 4k, less the memory management overhead
smallChunkSize :: Int
smallChunkSize = 4 * 1024 - chunkOverhead
{-# INLINE smallChunkSize #-}

-- | @defaultInitSize = 64 - chunkOverhead@
--
defaultInitSize :: Int
defaultInitSize = 64 - chunkOverhead
{-# INLINE defaultInitSize #-}

-- | The memory management overhead. Currently this is tuned for GHC only.
chunkOverhead :: Int
chunkOverhead = 2 * sizeOf (undefined :: Int)
{-# INLINE chunkOverhead #-}

-- | /O(n)/ Convert a list into a vector with an approximate size.
--
-- If the list's length is large than the size given, we simply double the buffer size
-- and continue building.
--
-- This function is a /good consumer/ in the sense of build/foldr fusion.
--
packN :: forall v a. Vec v a => Int -> [a] -> v a
packN n0 = \ ws0 -> runST (do mba <- newArr n0
                              (SP3 i _ mba') <- foldlM go (SP3 0 n0 mba) ws0
                              shrinkMutableArr mba' i
                              ba <- unsafeFreezeArr mba'
                              return $! fromArr ba 0 i
                          )
  where
    -- It's critical that this function get specialized and unboxed
    -- Keep an eye on its core!
    go :: SP3 (MArray v s a) -> a -> ST s (SP3 (MArray v s a))
    go (SP3 i n mba) !x =
        if i < n
        then do writeArr mba i x
                let !i' = i+1
                return (SP3 i' n mba)
        else do let !n' = (n + chunkOverhead) `shiftL` 1 - chunkOverhead
                    !i' = i+1
                !mba' <- resizeMutableArr mba n'
                writeArr mba' i x
                return (SP3 i' n' mba')
{-# INLINE packN #-}

-- | /O(n)/
--
-- Alias for @'packRN' 16@.
--
packR :: Vec v a => [a] -> v a
packR = packRN 16
{-# INLINE packR #-}

-- | /O(n)/ 'packN' in reverse order.
--
-- This function is a /good consumer/ in the sense of build/foldr fusion.
--
packRN :: forall v a. Vec v a => Int -> [a] -> v a
packRN n0 = \ ws0 -> runST (do mba <- newArr n0
                               let !n0' = n0-1
                               (SP3 i n mba') <- foldlM go (SP3 n0' n0 mba) ws0
                               ba <- unsafeFreezeArr mba'
                               return $! fromArr ba i (n-i)
                           )
  where
    go :: SP3 (MArray v s a) -> a -> ST s (SP3 (MArray v s a))
    go (SP3 i n mba) !x =
        if i >= 0
        then do writeArr mba i x
                let !i' = i-1
                return (SP3 i' n mba)
        else do let !n' = n `shiftL` 1
                    !n'' = n-1
                !mba' <- newArr n'
                copyMutableArr mba' n mba 0 n
                return (SP3 n'' n' mba')
data SP3 a = SP3 {-# UNPACK #-}!Int {-# UNPACK #-}!Int a
{-# INLINE packRN #-}

-- | /O(n)/ Convert vector to a list.
--
-- Unpacking is done lazily. i.e. we will retain reference to the array until all element are consumed.
--
-- This function is a /good producer/ in the sense of build/foldr fusion.
--
unpack :: Vec v a => v a -> [a]
unpack (VecPat ba s l) = List.map (indexArr ba) [s..s+l-1]
{-# INLINE unpack #-}

-- | /O(n)/ Convert vector to a list in reverse order.
--
-- This function is a /good producer/ in the sense of build/foldr fusion.
--
unpackR :: Vec v a => v a -> [a]
unpackR (VecPat ba s l) =
    let !sl = s + l
    in List.map (\ i -> indexArr ba (sl-i)) [1..l]
{-# INLINE unpackR #-}

--------------------------------------------------------------------------------
-- Basic interface
--
-- |  /O(1)/ The length of a vector.
--
length :: Vec v a => v a -> Int
length (VecPat _ _ l) = l
{-# INLINE length #-}

-- | /O(1)/ Test whether a vector is empty.
--
null :: Vec v a => v a -> Bool
null v = length v == 0
{-# INLINE null #-}

-- | /O(m+n)/
--
append :: Vec v a => v a -> v a -> v a
append (VecPat _ _ 0) b                    = b
append a                (VecPat _ _ 0)     = a
append (VecPat baA sA lA) (VecPat baB sB lB) = create (lA+lB) $ \ mba -> do
    copyArr mba 0  baA sA lA
    copyArr mba lA baB sB lB
{-# INLINE append #-}

-- | /O(n)/ 'cons' is analogous to (:) for lists, but of different
-- complexity, as it requires making a copy.
--
cons :: Vec v a => a -> v a -> v a
cons x (VecPat ba s l) = create (l+1) $ \ mba -> do
    writeArr mba 0 x
    copyArr mba 1 ba s l
{-# INLINE cons #-}

-- | /O(n)/ Append a byte to the end of a vector
--
snoc :: Vec v a => v a -> a -> v a
snoc (VecPat ba s l) x = create (l+1) $ \ mba -> do
    copyArr mba 0 ba s l
    writeArr mba l x
{-# INLINE snoc #-}

-- | /O(1)/ Extract the head and tail of a PrimVector, returning Nothing
-- if it is empty.
--
uncons :: Vec v a => v a -> Maybe (a, v a)
uncons (VecPat ba s l)
    | l <= 0    = Nothing
    | otherwise = let v = fromArr ba (s+1) (l-1) in v `seq` Just (indexArr ba s, v)
{-# INLINE uncons #-}

-- | /O(1)/ Extract the 'init' and 'last' of a PrimVector, returning Nothing
-- if it is empty.
--
unsnoc :: Vec v a => v a -> Maybe (v a, a)
unsnoc (VecPat ba s l)
    | l <= 0    = Nothing
    | otherwise = let v = fromArr ba s (l-1) in v `seq` Just (v, indexArr ba (s+l-1))
{-# INLINE unsnoc #-}

-- | /O(1)/ Extract the first element of a PrimVector, which must be non-empty.
-- An exception will be thrown in the case of an empty PrimVector.
--
head :: Vec v a => v a -> a
head (VecPat ba s l)
    | l <= 0    = errorEmptyVector "head"
    | otherwise = indexArr ba s
{-# INLINE head #-}

-- | /O(1)/ Extract the elements after the head of a PrimVector, which must be non-empty.
-- An exception will be thrown in the case of an empty PrimVector.
tail :: Vec v a => v a -> v a
tail (VecPat ba s l)
    | l <= 0    = errorEmptyVector "tail"
    | otherwise = fromArr ba (s+1) (l-1)
{-# INLINE tail #-}

-- | /O(1)/ Extract the first element of a PrimVector, which must be non-empty.
-- An exception will be thrown in the case of an empty PrimVector.
--
last :: Vec v a => v a -> a
last (VecPat ba s l)
    | l <= 0    = errorEmptyVector "last"
    | otherwise = indexArr ba (s+l-1)
{-# INLINE last #-}

-- | /O(1)/ Extract the elements after the head of a PrimVector, which must be non-empty.
-- An exception will be thrown in the case of an empty PrimVector.
init :: Vec v a => v a -> v a
init (VecPat ba s l)
    | l <= 0    = errorEmptyVector "init"
    | otherwise = fromArr ba s (l-1)
{-# INLINE init #-}

--------------------------------------------------------------------------------

map :: forall u v a b. (Vec u a, Vec v b) => (a -> b) -> u a -> v b
map f = \ (VecPat ba s l) -> create l (go ba (l+s) s 0)
  where
    go :: IArray u a -> Int -> Int -> Int -> MArray v s b -> ST s ()
    go !ba !sl !i !j !mba  | i >= sl = return ()
                           | otherwise = do x <- indexArrM ba i
                                            writeArr mba j (f x)
                                            go ba sl (i+1) (j+1) mba
{-# INLINE map #-}

-- | /O(n)/ 'reverse' @xs@ efficiently returns the elements of @xs@ in reverse order.
--
reverse :: forall v a. (Vec v a) => v a -> v a
reverse (VecPat ba s l) = create l (go s)
  where
    !sl = s + l -1
    go :: Int -> MArray v s a -> ST s ()
    go !i !mba | i > sl = return ()
               | otherwise = do let x = indexArr ba i
                                writeArr mba (sl-i) x
                                go (i+1) mba
{-# INLINE [1] reverse #-}
{-# RULES "reverse/Bytes" reverse = reverseBytes #-}

reverseBytes :: Bytes -> Bytes
reverseBytes (PrimVector (PrimArray (ByteArray ba#)) s l) =
    create l (\ (MutablePrimArray (MutableByteArray mba#)) ->
        unsafeIOToST (c_reverse mba# ba# (fromIntegral (s + l))))
{-# INLINE reverseBytes #-}

-- | /O(n)/ The 'intersperse' function takes a 'Word8' and a
-- 'PrimVector' and \`intersperses\' that byte between the elements of
-- the 'PrimVector'.  It is analogous to the intersperse function on
-- Lists.
--
intersperse :: forall v a. Vec v a => a -> v a -> v a
intersperse x v@(VecPat ba s l)
    | l < 2  = v
    | otherwise = create (2*l-1) (go s 0)
   where
    sl = s + l -1
    go :: Int -> Int -> MArray v s a -> ST s ()
    go !i !j !mba
        | i == sl = writeArr mba j =<< indexArrM ba i
        | otherwise = do
            writeArr mba j (indexArr ba i)
            writeArr mba (j+1) (indexArr ba i)
            go (i+1) (j+2) mba
{-# INLINE [1] intersperse #-}
{-# RULES "intersperse/Bytes" intersperse = intersperseBytes #-}

intersperseBytes :: Word8 -> Bytes -> Bytes
intersperseBytes w v@(PrimVector (PrimArray (ByteArray ba#)) s l)
    | l < 2  = v
    | otherwise = create (2*l-1) (\ (MutablePrimArray (MutableByteArray mba#)) ->
            unsafeIOToST
                (c_intersperse mba# ba# (fromIntegral s) (fromIntegral l) (fromIntegral w))
        )
{-# INLINE intersperseBytes #-}

-- | /O(n)/ The 'intercalate' function takes a 'PrimVector' and a list of
-- 'PrimVector's and concatenates the list after interspersing the first
-- argument between each element of the list.
--
-- Note: 'intercalate' will force the entire 'PrimVector' list.
--
intercalate :: Vec v a => v a -> [v a] -> v a
intercalate s = concat . List.intersperse s
{-# INLINE intercalate #-}

-- | /O(n)/ intercalateElem. An efficient way to join [PrimVector]
-- with an element. It's faster than @intercalate (singleton c)@.
--
intercalateElem :: Vec v a => a -> [v a] -> v a
intercalateElem w = \ vs -> create (len vs) (copy 0 vs)
  where
    len []                      = 0
    len [VecPat _ _ l]    = l
    len (VecPat _ _ l:vs) = l + 1 + len vs

    copy !i []                 !mba = return ()
    copy !i (VecPat ba s l:vs) !mba = do
        let !i' = i + l
        copyArr mba i ba s l
        copy i' vs mba
{-# INLINE intercalateElem #-}

-- | The 'transpose' function transposes the rows and columns of its
-- 'PrimVector' argument.
--
transpose :: Vec v a => [v a] -> [v a]
transpose vs =
    List.map (packN (List.length vs)) . List.transpose . List.map unpack $ vs
{-# INLINE transpose #-}

--------------------------------------------------------------------------------
--
-- Reducing vectors (folds)
--
foldl :: (Vec v a) => (b -> a -> b) -> b -> v a -> b
foldl f z = List.foldl f z . unpack
{-# INLINE foldl #-}

foldl' :: (Vec v a) => (b -> a -> b) -> b -> v a -> b
foldl' f z = \ (VecPat ba s l) -> go z s (s+l) ba
  where
    -- tail recursive; traverses array left to right
    go !acc !p !q ba | p >= q    = acc
                     | otherwise = go (f acc (indexArr ba p)) (p + 1) q ba
{-# INLINE foldl' #-}

foldl1' :: forall v a. Vec v a => (a -> a -> a) -> a -> v a -> a
foldl1' f z = \ (VecPat ba s l) ->
    if l <= 0 then errorEmptyVector "foldl1'"
              else foldl' f (indexArr ba s) (fromArr ba (s+1) (l-1) :: v a)
{-# INLINE foldl1' #-}


foldr :: Vec v a => (a -> b -> b) -> b -> v a -> b
foldr f z = List.foldr f z . unpack
{-# INLINE foldr #-}

foldr' :: Vec v a => (a -> b -> b) -> b -> v a -> b
foldr' f z =  \ (VecPat ba s l) -> go z (s+l-1) s ba
  where
    -- tail recursive; traverses array right to left
    go !acc !p !q ba | p < q     = acc
                     | otherwise = go (f (indexArr ba p) acc) (p - 1) q ba
{-# INLINE foldr' #-}

foldr1' :: forall v a. Vec v a => (a -> a -> a) -> a -> v a -> a
foldr1' f z = \ (VecPat ba s l) ->
    if l <= 0 then errorEmptyVector "foldr1'"
              else foldl' f (indexArr ba (s+l-1)) (fromArr ba s (l-1) :: v a)
{-# INLINE foldr1' #-}

--------------------------------------------------------------------------------
--
-- Special folds
--
-- | /O(n)/ Concatenate a list of primitive vector.
--
-- Note: 'concat' have to force the entire list to filter out empty 'PrimVector' and calculate
-- the length for allocation.
--
concat :: forall v a . Vec v a => [v a] -> v a
concat vs = case pre 0 0 vs of
    (0, _)  -> empty
    (1, _)  -> let Just v = List.find (not . null) vs in v
    (_, l') -> create l' (copy vs l')
  where
    -- pre scan to decide if we really need to copy and calculate total length
    pre :: Int -> Int -> [v a] -> (Int, Int)
    pre !nacc !lacc [] = (nacc, lacc)
    pre !nacc !lacc (v@(VecPat _ _ l):vs)
        | l <= 0    = pre nacc lacc vs
        | otherwise = pre (nacc+1) (l+lacc) vs

    copy :: [v a] -> Int -> MArray v s a -> ST s ()
    copy [] _ _           = return ()
    copy (v:vs) !i !mba = do let (ba, s, l) = toArr v
                                 !i' = i - l
                             when (l /= 0) (copyArr mba i' ba s l)
                             copy vs i' mba
{-# INLINE concat #-}

-- | Map a function over a 'PrimVector' and concatenate the results
concatMap :: Vec v a => (a -> v a) -> v a -> v a
concatMap f = concat . foldr' ((:) . f) []
{-# INLINE concatMap #-}

-- | /O(n)/ Applied to a predicate and a PrimVector, 'any' determines if
-- any element of the 'PrimVector' satisfies the predicate.
any :: Vec v a => (a -> Bool) -> v a -> Bool
any f = List.any f . unpack
{-# INLINE any #-}

-- | /O(n)/ Applied to a predicate and a 'PrimVector', 'all' determines
-- if all elements of the 'PrimVector' satisfy the predicate.
all :: Vec v a => (a -> Bool) -> v a -> Bool
all f = List.all f . unpack
{-# INLINE all #-}

-- | /O(n)/ 'maximum' returns the maximum value from a 'PrimVector'
-- This function will fuse.
-- An exception will be thrown in the case of an empty PrimVector.
maximum :: (Vec v a, Ord a) => v a -> a
maximum = undefined

-- | /O(n)/ 'minimum' returns the minimum value from a 'PrimVector'
-- This function will fuse.
-- An exception will be thrown in the case of an empty PrimVector.
minimum :: (Vec v a, Ord a) => v a -> a
minimum = undefined

--------------------------------------------------------------------------------
-- Scans

-- | 'scanl' is similar to 'foldl', but returns a list of successive
-- reduced values from the left.
--
-- > scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
--
-- Note that
--
-- > last (scanl f z xs) == foldl f z xs.
--
scanl :: forall v u a b. (Vec v a, Vec u b) => (b -> a -> b) -> b -> v a -> u b
scanl f z = \ (VecPat ba s l) ->
    create (l+1) (\ mba -> writeArr mba 0 z >> go ba z s 1 l mba)
  where
    go :: IArray v a -> b -> Int -> Int -> Int -> MArray u s b -> ST s ()
    go !ba !acc !i !j !l !mba
        | j > l = return ()
        | otherwise = do
            let acc' = acc `f` (indexArr ba i)
            writeArr mba j acc'
            go ba acc' (i+1) (j+1) l mba
{-# INLINE scanl #-}

-- | 'scanl1' is a variant of 'scanl' that has no starting value argument.
-- This function will fuse.
--
-- > scanl1 f [x1, x2, ...] == [x1, x1 `f` x2, ...]
--
scanl1 :: forall v a. Vec v a => (a -> a -> a) -> v a -> v a
scanl1 f = \ (VecPat ba s l) ->
    if l <= 0 then errorEmptyVector "scanl1"
              else scanl f (indexArr ba s) (fromArr ba (s+1) (l-1) :: v a)
{-# INLINE scanl1 #-}

-- | scanr is the right-to-left dual of scanl.
--
scanr :: forall v u a b. (Vec v a, Vec u b) => (a -> b -> b) -> b -> v a -> u b
scanr f z = \ (VecPat ba s l) ->
    create (l+1) (\ mba -> writeArr mba l z >> go ba z (s+l-1) (l-1) mba)
  where
    go :: IArray v a -> b -> Int -> Int -> MArray u s b -> ST s ()
    go !ba !acc !i !j !mba
        | j < 0 = return ()
        | otherwise = do
            let acc' = indexArr ba i `f` acc
            writeArr mba j acc'
            go ba acc' (i-1) (j-1) mba
{-# INLINE scanr #-}

-- | 'scanr1' is a variant of 'scanr' that has no starting value argument.
scanr1 :: forall v a. Vec v a => (a -> a -> a) -> v a -> v a
scanr1 f = \ (VecPat ba s l) ->
    if l <= 0 then errorEmptyVector "scanl1"
              else scanr f (indexArr ba (s+l-1)) (fromArr ba s (l-1) :: v a)
{-# INLINE scanr1 #-}

--------------------------------------------------------------------------------
-- Accumulating maps

-- | The 'mapAccumL' function behaves like a combination of 'map' and
-- 'foldl'; it applies a function to each element of a primitive vector,
-- passing an accumulating parameter from left to right, and returning a
-- final value of this accumulator together with the new list.
--
mapAccumL :: forall u v a b c. (Vec u b, Vec v c) => (a -> b -> (a, c)) -> a -> u b -> (a, v c)
mapAccumL f z = \ (VecPat ba s l) -> creating l (go z s 0 (s+l) ba)
  where
    go :: a -> Int -> Int -> Int -> IArray u b -> MArray v s c -> ST s a
    go !acc !i !j !sl !ba !mba
        | i >= sl   = return acc
        | otherwise = do
            let (acc', c) = acc `f` indexArr ba i
            writeArr mba j c
            go acc' (i+1) (j+1) sl ba mba
{-# INLINE mapAccumL #-}

-- | The 'mapAccumR' function behaves like a combination of 'map' and
-- 'foldr'; it applies a function to each element of a primitive vector,
-- passing an accumulating parameter from right to left, and returning a
-- final value of this accumulator together with the new primitive vector.
--
mapAccumR :: forall u v a b c. (Vec u b, Vec v c) => (a -> b -> (a, c)) -> a -> u b -> (a, v c)
mapAccumR f z = \ (VecPat ba s l) -> creating l (go z (s+l-1) s ba)
  where
    go :: a -> Int -> Int -> IArray u b -> MArray v s c -> ST s a
    go !acc !i !s !ba !mba
        | i < s     = return acc
        | otherwise = do
            let (acc', c) = acc `f` indexArr ba i
            writeArr mba (i-s) c
            go acc' (i-1) s ba mba
{-# INLINE mapAccumR #-}

--  Generating and unfolding primitive vector
--
---- | /O(n)/ 'replicate' @n x@ is a primitive vector of length @n@ with @x@
-- the value of every element. The following holds:
--
-- > replicate w c = unfoldr w (\u -> Just (u,u)) c
--
-- This implemenation uses @setByteArray#@.
--
replicate :: (Vec v a) => Int -> a -> v a
replicate n x = create n (\ mba -> setArr mba 0 n x)
{-# INLINE replicate #-}

-- | /O(n)/, where /n/ is the length of the result.  The 'unfoldr'
-- function is analogous to the List \'unfoldr\'.  'unfoldr' builds a
-- primitive vector from a seed value. The function takes the element and
-- returns 'Nothing' if it is done producing the primitive vector or returns
-- 'Just' @(a,b)@, in which case, @a@ is the next byte in the string,
-- and @b@ is the seed value for further production.
--
-- Examples:
--
-- >    unfoldr (\x -> if x <= 5 then Just (x, x + 1) else Nothing) 0
-- > == pack [0, 1, 2, 3, 4, 5]
--
unfoldr :: Vec u b => (a -> Maybe (b, a)) -> a -> u b
unfoldr f = pack . List.unfoldr f
{-# INLINE unfoldr #-}

-- | /O(n)/ Like 'unfoldr', 'unfoldrN' builds a ByteString from a seed
-- value.  However, the length of the result is limited by the first
-- argument to 'unfoldrN'.  This function is more efficient than 'unfoldr'
-- when the maximum length of the result is known.
--
-- The following equation relates 'unfoldrN' and 'unfoldr':
--
-- > fst (unfoldrN n f s) == take n (unfoldr f s)
--
unfoldrN :: forall v a b. Vec v b => Int -> (a -> Maybe (b, a)) -> a -> (v b, Maybe a)
unfoldrN n f
    | n < 0     = \ z -> (empty, Just z)
    | otherwise = \ z ->
        let ((r, len), v) = creating n (go z 0)
        in (take len v, r)
  where
    go :: a -> Int -> MArray v s b -> ST s (Maybe a, Int)
    go !acc !i !mba
      | n == i    = return (Just acc, i)
      | otherwise = case f acc of
          Nothing        -> return (Nothing, i)
          Just (x, acc') -> do writeArr mba i x
                               go acc' (i+1) mba
{-# INLINE unfoldrN #-}

-- ---------------------------------------------------------------------
-- Substrings

-- | /O(1)/ 'take' @n@, applied to a ByteString @xs@, returns the prefix
-- of @xs@ of length @n@, or @xs@ itself if @n > 'length' xs@.
take :: Vec v a => Int -> v a -> v a
take n v@(VecPat ba s l)
    | n <= 0    = empty
    | n >= l    = v
    | otherwise = fromArr ba s n
{-# INLINE take #-}

-- | /O(1)/ 'drop' @n xs@ returns the suffix of @xs@ after the first @n@
-- elements, or @[]@ if @n > 'length' xs@.
drop :: Vec v a => Int -> v a -> v a
drop n v@(VecPat ba s l)
    | n <= 0    = v
    | n >= l    = empty
    | otherwise = fromArr ba (s+n) (l-n)
{-# INLINE drop #-}


-- | /O(1)/ Extract a sub-range vector with give start index and length.
--
-- This function is a total function just like 'take/drop', e.g.
--
-- @
-- slice 1 3 "hello"   == "ell"
-- slice -1 -1 "hello" == ""
-- slice 2 10 "hello"  == "llo"
-- @
--
slice :: Vec v a => Int -> Int -> v a -> v a
slice s' l' (VecPat arr s l) | l'' == 0  = empty
                             | otherwise = fromArr arr s'' l''
  where
    s'' = rangeCut (s+s') s (s+l)
    l'' = rangeCut l 0 (s+l-s'')

-- | /O(1)/ Extract a sub-range vector with give start and end, both start and end index
-- can be negative which stand for counting from the end(similar to the slicing operator([..])
-- in many other language).
--
-- This function is a total function just like 'take/drop', e.g.
--
-- @
-- "hello" |..| (1, 3) == "el"
-- "hello" |..| (1, -1) == "ell"
-- slice "hello" (-3, -2) == "l"
-- slice "hello" (-3, -4) == ""
-- @
--
(|..|) :: Vec v a => v a -> (Int, Int) -> v a
(VecPat arr s l) |..| (s1, s2) | s1' <= s2' = empty
                               | otherwise  = fromArr arr s1' (s2' - s1')
  where
    s1' = rangeCut (if s1>0 then s+s1 else s+l+s1) s (s+l)
    s2' = rangeCut (if s2>0 then s+s2 else s+l+s2) s (s+l)

-- | /O(1)/ 'splitAt' @n xs@ is equivalent to @('take' n xs, 'drop' n xs)@.
splitAt :: Vec v a => Int -> v a -> (v a, v a)
splitAt s' (VecPat arr s l) = let v1 = fromArr arr s'' (s''-s)
                                  v2 = fromArr arr s'' (s+l-s'')
                              in v1 `seq` v2 `seq` (v1, v2)
  where s'' = rangeCut (s+s') s (s+l)


--------------------------------------------------------------------------------
-- Common up near identical calls to `error' to reduce the number
-- constant strings created when compiled:
errorEmptyVector :: String -> a
errorEmptyVector fun = error (moduleErrorMsg fun "empty PrimVector")
{-# NOINLINE errorEmptyVector #-}

moduleErrorMsg :: String -> String -> String
moduleErrorMsg fun msg = "Data.PrimVector." ++ fun ++ ':':' ':msg

rangeCut :: Int -> Int -> Int -> Int
rangeCut !r !min !max | r < min = min
                      | r > max = max
                      | otherwise = r
{-# INLINE rangeCut #-}

--------------------------------------------------------------------------------

foreign import ccall unsafe "string.h strlen" c_strlen
    :: Addr# -> IO CSize

foreign import ccall unsafe "bytes.c reverse"
    c_reverse :: MutableByteArray# s -> ByteArray# -> CSize -> IO ()

foreign import ccall unsafe "bytes.c intersperse"
    c_intersperse :: MutableByteArray# s -> ByteArray# -> CSize -> CSize -> CChar -> IO ()

foreign import ccall unsafe "bytes.c _memcmp"
    c_memcmp :: ByteArray# -> CSize -> ByteArray# -> CSize -> CSize -> IO CInt
