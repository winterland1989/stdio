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
  , empty, singleton, copy
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
  , foldl'
  , foldl1'
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
  , IPair(..)
  , defaultInitSize
  , chunkOverhead
  , defaultChunkSize
  , smallChunkSize
 ) where

import Control.DeepSeq
import Control.Exception (assert)
import GHC.Exts (IsList(..), IsString(..), build)
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
import Data.Primitive.PrimArrayQ as Q
import Debug.Trace

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
    !endA = sA + lA
    go !i !j
        | i < endA =
            (indexSmallArray baA i == indexSmallArray baB j) && go (i+1) (j+1)
        | otherwise = True

instance {-# OVERLAPPABLE #-} Ord a => Ord (Vector a) where
    compare = compareVector
    {-# INLINABLE compare #-}

compareVector :: Ord a => Vector a -> Vector a -> Ordering
compareVector (Vector baA sA lA) (Vector baB sB lB) = go sA sB
  where
    !endA = sA + lA
    !endB = sB + lB
    go !i !j | i >= endA  = endB `compare` j
             | j >= endB  = endA `compare` i
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
    (Q.word16LiteralLE $ \ len addr -> [| PrimVector (Q.word16ArrayFromAddr len $(addr)) 0 len |])
    (error "Cannot use bAsc as a pattern")
    (error "Cannot use bAsc as a type")
    (error "Cannot use bAsc as a dec")

instance Prim a => Vec PrimVector a where
    type MArray PrimVector = MutablePrimArray
    type IArray PrimVector = PrimArray
    toArr (PrimVector arr s l) = (arr, s, l)
    {-# INLINE toArr #-}
    fromArr arr s l = PrimVector arr s l
    {-# INLINE fromArr #-}

instance {-# OVERLAPPABLE #-} Prim a => Eq (PrimVector a) where
    (==) = eqPrimVector
    {-# INLINE (==) #-}

eqPrimVector :: forall a. Prim a => PrimVector a -> PrimVector a -> Bool
{-# INLINE eqPrimVector #-}
eqPrimVector (PrimVector (PrimArray (ByteArray baA#)) sA lA)
        (PrimVector (PrimArray (ByteArray baB#)) sB lB) =
    let r = unsafeDupablePerformIO $
            c_memcmp baA# (fromIntegral $ sA * siz) -- we use memcmp for all primitive vector
                     baB# (fromIntegral $ sB * siz) (fromIntegral $ min (lA*siz) (lB*siz))
    in lA == lB && r == 0
  where siz = sizeOf (undefined :: a)

instance {-# OVERLAPPABLE #-} (Prim a, Ord a) => Ord (PrimVector a) where
    compare = comparePrimVector
    {-# INLINE compare #-}

instance {-# OVERLAPPING #-} Ord (PrimVector Word8) where
    compare = compareBytes
    {-# INLINE compare #-}

comparePrimVector :: (Prim a, Ord a) => PrimVector a -> PrimVector a -> Ordering
{-# INLINE comparePrimVector #-}
comparePrimVector (PrimVector baA sA lA) (PrimVector baB sB lB) = go sA sB
  where
    !endA = sA + lA
    !endB = sB + lB
    go !i !j | i >= endA  = endB `compare` j
             | j >= endB  = endA `compare` i
             | otherwise = let o = indexPrimArray baA i `compare` indexPrimArray baB j
                           in case o of EQ -> go (i+1) (j+1)
                                        x  -> x

compareBytes :: PrimVector Word8 -> PrimVector Word8 -> Ordering
{-# INLINE compareBytes #-}
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
-- If the initialization function return a length larger than initial size, an error will be raised.
--
createN :: Vec v a
        => Int  -- length's upper bound
        -> (forall s. MArray v s a -> ST s Int)  -- initialization function which return the actual length
        -> v a
{-# INLINE createN #-}
createN l fill = runST (do
        mba <- newArr l
        l' <- fill mba
        shrinkMutableArr mba l'
        ba <- unsafeFreezeArr mba
        if l' <= l
        then return $! fromArr ba 0 l'
        else error "Data.PrimVector.createN: return size exceeds initial size"
    )

-- | /O(1)/. The empty vector.
--
empty :: Vec v a => v a
{-# INLINE empty #-}
empty = create 0 (\_ -> return ())

-- | /O(1)/. Single element vector.
singleton :: Vec v a => a -> v a
{-# INLINE singleton #-}
singleton c = create 1 (\ mba -> writeArr mba 0 c)

-- | /O(n)/. Copy a vector from slice.
--
copy :: Vec v a => v a -> v a
{-# INLINE copy #-}
copy (VecPat ba s l) = create l (\ mba -> copyArr mba 0 ba s l)

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

-- | @defaultInitSize = 30
--
defaultInitSize :: Int
defaultInitSize = 30
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
                              (IPair i mba') <- foldlM go (IPair 0 mba) ws0
                              shrinkMutableArr mba' i
                              ba <- unsafeFreezeArr mba'
                              return $! fromArr ba 0 i
                          )
  where
    -- It's critical that this function get specialized and unboxed
    -- Keep an eye on its core!
    go :: IPair (MArray v s a) -> a -> ST s (IPair (MArray v s a))
    go (IPair i mba) x = do
        n <- sizeofMutableArr mba
        if i < n
        then do writeArr mba i x
                return (IPair (i+1) mba)
        else do let !n' = n `shiftL` 1
                !mba' <- resizeMutableArr mba n'
                writeArr mba' i x
                return (IPair (i+1) mba')

data IPair a = IPair {-# UNPACK #-}!Int a
{-# INLINE packN #-}

-- | /O(n)/
--
-- Alias for @'packRN' 16@.
--
packR :: Vec v a => [a] -> v a
packR = packRN defaultInitSize
{-# INLINE packR #-}

-- | /O(n)/ 'packN' in reverse order.
--
-- This function is a /good consumer/ in the sense of build/foldr fusion.
--
packRN :: forall v a. Vec v a => Int -> [a] -> v a
packRN n0 = \ ws0 -> runST (do mba <- newArr n0
                               let !n0' = n0-1
                               (IPair i mba') <- foldlM go (IPair n0' mba) ws0
                               ba <- unsafeFreezeArr mba'
                               let n = sizeofArr ba
                               return $! fromArr ba i (n-i)
                           )
  where
    go :: IPair (MArray v s a) -> a -> ST s (IPair (MArray v s a))
    go (IPair i mba) !x = do
        n <- sizeofMutableArr mba
        if i >= 0
        then do writeArr mba i x
                let !i' = i-1
                return (IPair i' mba)
        else do let !n' = n `shiftL` 1
                    !n'' = n' - n
                !mba' <- newArr n'
                copyMutableArr mba' n'' mba 0 n
                writeArr mba' n'' x
                return (IPair (n'' - 1) mba')
{-# INLINE packRN #-}

-- | /O(n)/ Convert vector to a list.
--
-- Unpacking is done lazily. i.e. we will retain reference to the array until all element are consumed.
--
-- This function is a /good producer/ in the sense of build/foldr fusion.
--
unpack :: Vec v a => v a -> [a]
{-# INLINE [1] unpack #-}
unpack (VecPat ba s l) = go s
  where
    !end = s + l
    go !idx
        | idx >= end = []
        | otherwise = let !x = indexArr ba idx in x : go (idx+1)

unpackFB :: Vec v a => v a -> (a -> r -> r) -> r -> r
{-# INLINE [0] unpackFB #-}
unpackFB (VecPat ba s l) k z = go s
  where
    !end = s + l
    go !idx
        | idx >= end = z
        | otherwise = let !x = indexArr ba idx in x `k` go (idx+1)

{-# RULES
"unpack" [~1] forall v . unpack v = build (\ k z -> unpackFB v k z)
"unpackFB" [1] forall v . unpackFB v (:) [] = unpack v
 #-}


-- | /O(n)/ Convert vector to a list in reverse order.
--
-- This function is a /good producer/ in the sense of build/foldr fusion.
--
unpackR :: Vec v a => v a -> [a]
unpackR (VecPat ba s l) = go (s + l - 1)
  where
    go !idx
        | idx < s = []
        | otherwise =
            let !x = indexArr ba idx in x : go (idx-1)
{-# INLINE [1] unpackR #-}

unpackRFB :: Vec v a => v a -> (a -> r -> r) -> r -> r
{-# INLINE [0] unpackRFB #-}
unpackRFB (VecPat ba s l) k z = go (s + l - 1)
  where
    go !idx
        | idx < s = z
        | otherwise =
            let !x = indexArr ba idx in x `k` go (idx-1)
{-# RULES
"unpackR" [~1] forall v . unpackR v = build (\ k z -> unpackRFB v k z)
"unpackRFB" [1] forall v . unpackRFB v (:) [] = unpackR v
 #-}

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
map f = \ (VecPat ba s l) -> create l (go ba l s 0)
  where
    go :: IArray u a -> Int -> Int -> Int -> MArray v s b -> ST s ()
    go !ba !l !i !j !mba | j >= l = return ()
                         | otherwise = do x <- indexArrM ba i
                                          writeArr mba j (f x)
                                          go ba l (i+1) (j+1) mba
{-# INLINE map #-}

-- | /O(n)/ 'reverse' @xs@ efficiently returns the elements of @xs@ in reverse order.
--
reverse :: forall v a. (Vec v a) => v a -> v a
reverse = \ (VecPat ba s l) -> create l (go ba s (l-1))
  where
    go :: IArray v a -> Int -> Int -> MArray v s a -> ST s ()
    go ba !i !j !mba | j < 0 = return ()
                     | j > 4 = do  -- a bit of loop unrolling
                         indexArrM ba i >>= writeArr mba j
                         indexArrM ba (i+1) >>= writeArr mba (j-1)
                         indexArrM ba (i+2) >>= writeArr mba (j-2)
                         indexArrM ba (i+3) >>= writeArr mba (j-3)
                         go ba (i+4) (j-4) mba
                     | otherwise = do indexArrM ba i >>= writeArr mba j
                                      go ba (i+1) (j-1) mba
{-# INLINE reverse #-}

-- | /O(n)/ The 'intersperse' function takes an element and a
-- vector and \`intersperses\' that element between the elements of
-- the vector.  It is analogous to the intersperse function on
-- Lists.
--
intersperse :: forall v a. Vec v a => a -> v a -> v a
intersperse x = \ v@(VecPat ba s l) ->
    if l < 2  then v else create (2*l-1) (go ba s 0 (s+l-1))
   where
    go :: IArray v a  -- the original bytes
       -> Int         -- the reading index of orginal bytes
       -> Int         -- the writing index of new buf
       -> Int         -- the end of reading index(point to the last byte)
       -> MArray v s a -- the new buf
       -> ST s ()
    go ba !i !j !end !mba
        | i >= end = writeArr mba j =<< indexArrM ba i
        | i < end - 4 = do -- a bit of loop unrolling
            writeArr mba j =<< indexArrM ba i
            writeArr mba (j+1) x
            writeArr mba (j+2) =<< indexArrM ba (i+1)
            writeArr mba (j+3) x
            writeArr mba (j+4) =<< indexArrM ba (i+2)
            writeArr mba (j+5) x
            writeArr mba (j+6) =<< indexArrM ba (i+3)
            writeArr mba (j+7) x
            go ba (i+4) (j+8) end mba
        | otherwise = do
            writeArr mba j =<< indexArrM ba i
            writeArr mba (j+1) x
            go ba (i+1) (j+2) end mba
{-# INLINE intersperse #-}

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
    go !acc !i !j !end !ba !mba
        | i >= end = return acc
        | otherwise = do
            let (acc', c) = acc `f` indexArr ba i
            writeArr mba j c
            go acc' (i+1) (j+1) end ba mba
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
errorEmptyVector fun = error ("Data.PrimVector." ++ fun ++ ": empty PrimVector")
{-# NOINLINE errorEmptyVector #-}

rangeCut :: Int -> Int -> Int -> Int
rangeCut !r !min !max | r < min = min
                      | r > max = max
                      | otherwise = r
{-# INLINE rangeCut #-}

--------------------------------------------------------------------------------

foreign import ccall unsafe "bytes.c _memcmp"
    c_memcmp :: ByteArray# -> CSize -> ByteArray# -> CSize -> CSize -> IO CInt
