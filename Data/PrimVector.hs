{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface, CApiFFI #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- |
--
--
--
--
--

module Data.PrimVector
  ( -- * The 'PrimVector' type
    PrimVector(..)
    -- * Creating 'PrimVector' and conversion between list
  , create, creating, createN
  , empty
  , singleton
  , pack, packN, packR, packRN
  , unpack, unpackR
    -- * 'Word8' vector
  , Bytes
  , w2c, c2w
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

  ) where

import GHC.Prim
import GHC.Types
import GHC.ST
import Data.PrimArray
import Data.Primitive
import qualified Data.List as List

#if MIN_VERSION_base(4,9,0)
import Data.Semigroup (Semigroup((<>)))
#endif
import Data.Monoid (Monoid(..))

import Data.Data
import Data.Typeable
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

-- | Primitive vector
--
data PrimVector a = PrimVector
    {-# UNPACK #-} !(PrimArray a) -- payload
    {-# UNPACK #-} !Int         -- offset in elements of type a rather than in bytes
    {-# UNPACK #-} !Int         -- length in elements of type a rather than in bytes
  deriving Typeable


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
    compare = compareVector
    {-# INLINABLE compare #-}

instance {-# OVERLAPPING #-} Ord (PrimVector Word8) where
    compare = compareBytes
    {-# INLINABLE compare #-}

compareVector :: (Prim a, Ord a) => PrimVector a -> PrimVector a -> Ordering
compareVector (PrimVector baA sA lA) (PrimVector baB sB lB) = go sA sB
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
    showsPrec p v r = showsPrec p (unpack v) r

instance (Prim a, Read a) => Read (PrimVector a) where
    readsPrec p str = [ (pack x, y) | (x, y) <- readsPrec p str ]

instance (Prim a, Data a) => Data (PrimVector a) where
    gfoldl f z txt = z pack `f` unpack txt
    toConstr _     = error "Data.PrimVector.toConstr"
    gunfold _ _    = error "Data.PrimVector.gunfold"
    dataTypeOf _   = mkNoRepType "Data.PrimVector"

--------------------------------------------------------------------------------
-- Creating 'PrimVector' and conversion between list
--
-- | Create a 'PrimVector'.
--
create :: Prim a
       => Int  -- length in elements of type @a@
       -> (forall s. MutablePrimArray s a -> ST s ())  -- initialization function
       -> PrimVector a
create l fill = runST (do
        mba <- newPrimArray l
        fill mba
        ba <- unsafeFreezePrimArray mba
        return (PrimVector ba 0 l)
    )
{-# INLINE create #-}

-- | Create a 'PrimVector', return both the vector and the monadic result.
--
creating :: Prim a
         => Int  -- length in elements of type @a@
         -> (forall s. MutablePrimArray s a -> ST s b)  -- initialization function
         -> (b, PrimVector a)
creating l fill = runST (do
        mba <- newPrimArray l
        b <- fill mba
        ba <- unsafeFreezePrimArray mba
        return (b, PrimVector ba 0 l)
    )
{-# INLINE creating #-}

-- | Create a 'PrimVector' up to a specific length.
--
createN :: Prim a
        => Int  -- length's upper bound
        -> (forall s. MutablePrimArray s a -> ST s Int)  -- initialization function which return the actual length
                                                         -- (must be smaller than upper bound)
        -> PrimVector a
createN l fill = runST (do
        mba <- newPrimArray l
        l' <- fill mba
        ba <- unsafeFreezePrimArray mba
        assert (l' <= l) $ return (PrimVector ba 0 l')
    )
{-# INLINE createN #-}

-- | /O(1)/. The empty 'PrimVector'.
--
empty :: Prim a => PrimVector a
empty = create 0 (\_ -> return ())

-- | /O(1)/. Single element 'PrimVector'.
singleton :: Prim a => a -> PrimVector a
singleton c = create 1 (\ mba -> writePrimArray mba 0 c)
{-# INLINE singleton #-} -- Inline [1] for intercalate rule

-- | /O(n)/ Convert a list into a 'PrimVector'
--
-- Alias for @'packN' 16@.
--
pack :: Prim a => [a] -> PrimVector a
pack = packN 16
{-# INLINE pack #-}

-- | /O(n)/ Convert a list into a 'PrimVector' with an approximate size.
--
-- If the list's length is large than the size given, we simply double the buffer size
-- and continue building.
--
-- This function is a /good consumer/ in the sense of build/foldr fusion.
--
packN :: Prim a => Int -> [a] -> PrimVector a
packN n0 = \ ws0 -> runST (do mba <- newPrimArray n0
                              SP3 i _ mba' <- foldM go (SP3 0 n0 mba) ws0
                              shrinkMutablePrimArray mba' i
                              ba <- unsafeFreezePrimArray mba'
                              return (PrimVector ba 0 i)
                          )
  where
    -- It's critical that this function get specialized and unboxed
    go :: Prim a => SP3 s a -> a -> ST s (SP3 s a)
    go (SP3 i n mba) x =
        if i < n
        then do writePrimArray mba i x
                return (SP3 (i+1) n mba)
        else do let !n' = n `shiftL` 1
                !mba' <- resizeMutablePrimArray mba n'
                writePrimArray mba' i x
                return (SP3 (i+1) n' mba')

data SP3 s a = SP3 !Int  -- helper type to help ghc to specialize
                   !Int
                   !(MutablePrimArray s a)
{-# INLINE packN #-}

-- | /O(n)/
--
-- Alias for @'packRN' 16@.
--
packR :: Prim a => [a] -> PrimVector a
packR = packRN 16
{-# INLINE packR #-}

-- | /O(n)/ 'packN' in reverse order.
--
-- This function is a /good consumer/ in the sense of build/foldr fusion.
--
packRN :: Prim a => Int -> [a] -> PrimVector a
packRN n0 = \ ws0 -> runST (do mba <- newPrimArray n0
                               SP3 i n mba' <- foldM go (SP3 (n0-1) n0 mba) ws0
                               ba <- unsafeFreezePrimArray mba'
                               return (PrimVector ba i (n-i))
                           )
  where
    go :: Prim a => SP3 s a -> a -> ST s (SP3 s a)
    go (SP3 i n mba) x =
        if i >= 0
        then do writePrimArray mba i x
                return (SP3 (i-1) n mba)
        else do let !n' = n `shiftL` 1
                !mba' <- newPrimArray n'
                copyMutablePrimArray mba' n mba 0 n
                return (SP3 (n-1) n' mba')
{-# INLINE packRN #-}

-- | /O(n)/ Convert 'PrimVector' to a list.
--
unpack :: Prim a => PrimVector a -> [a]
unpack (PrimVector ba s l) = List.map (indexPrimArray ba) [s..s+l-1]
{-# INLINE unpack #-}

-- | /O(n)/ Convert 'PrimVector' to a list in reverse order.
--
unpackR :: Prim a => PrimVector a -> [a]
unpackR (PrimVector ba s l) = List.map (\ i -> indexPrimArray ba (sl-i)) [1..l]
  where !sl = s + l
{-# INLINE unpackR #-}

--------------------------------------------------------------------------------
-- Basic interface
--
-- |  /O(1)/ The length of a 'PrimVector'.
--
length :: PrimVector a -> Int
length (PrimVector _ _ l) = l
{-# INLINE length #-}

-- | /O(1)/ Test whether a 'PrimVector' is empty.
null :: PrimVector a -> Bool
null v = length v == 0
{-# INLINE null #-}

-- | /O()/
append :: Prim a => PrimVector a -> PrimVector a -> PrimVector a
append (PrimVector _ _  0)  b            = b
append a              (PrimVector _ _ 0) = a
append (PrimVector baA sA lA) (PrimVector baB sB lB) = create (lA+lB) $ \ mba -> do
    copyPrimArray mba 0  baA sA lA
    copyPrimArray mba lA baB sB lB
{-# INLINE append #-}

-- | /O(n)/ 'cons' is analogous to (:) for lists, but of different
-- complexity, as it requires making a copy.
--
cons :: Prim a => a -> PrimVector a -> PrimVector a
cons x (PrimVector ba s l) = create (l+1) $ \ mba -> do
    writePrimArray mba 0 x
    copyPrimArray mba 1 ba s l
{-# INLINE cons #-}

-- | /O(n)/ Append a byte to the end of a 'PrimVector'
--
snoc :: Prim a => PrimVector a -> a -> PrimVector a
snoc (PrimVector ba s l) x = create (l+1) $ \ mba -> do
    copyPrimArray mba 0 ba s l
    writePrimArray mba l x
{-# INLINE snoc #-}

-- | /O(1)/ Extract the head and tail of a PrimVector, returning Nothing
-- if it is empty.
--
uncons :: Prim a => PrimVector a -> Maybe (a, PrimVector a)
uncons (PrimVector ba s l)
    | l <= 0    = Nothing
    | otherwise = Just (indexPrimArray ba s, PrimVector ba (s+1) (l-1))
{-# INLINE uncons #-}

-- | /O(1)/ Extract the 'init' and 'last' of a PrimVector, returning Nothing
-- if it is empty.
--
unsnoc :: Prim a => PrimVector a -> Maybe (PrimVector a, a)
unsnoc (PrimVector ba s l)
    | l <= 0    = Nothing
    | otherwise = Just (PrimVector ba s (l-1), indexPrimArray ba (s+l-1))
{-# INLINE unsnoc #-}

-- | /O(1)/ Extract the first element of a PrimVector, which must be non-empty.
-- An exception will be thrown in the case of an empty PrimVector.
--
head :: Prim a => PrimVector a -> a
head (PrimVector ba s l)
    | l <= 0    = errorEmptyVector "head"
    | otherwise = indexPrimArray ba s
{-# INLINE head #-}

-- | /O(1)/ Extract the elements after the head of a PrimVector, which must be non-empty.
-- An exception will be thrown in the case of an empty PrimVector.
tail :: Prim a => PrimVector a -> PrimVector a
tail (PrimVector ba s l)
    | l <= 0    = errorEmptyVector "tail"
    | otherwise = PrimVector ba (s+1) (l-1)
{-# INLINE tail #-}

-- | /O(1)/ Extract the first element of a PrimVector, which must be non-empty.
-- An exception will be thrown in the case of an empty PrimVector.
--
last :: Prim a => PrimVector a -> a
last (PrimVector ba s l)
    | l <= 0    = errorEmptyVector "last"
    | otherwise = indexPrimArray ba (s+l-1)
{-# INLINE last #-}

-- | /O(1)/ Extract the elements after the head of a PrimVector, which must be non-empty.
-- An exception will be thrown in the case of an empty PrimVector.
init :: Prim a => PrimVector a -> PrimVector a
init (PrimVector ba s l)
    | l <= 0    = errorEmptyVector "init"
    | otherwise = PrimVector ba s (l-1)
{-# INLINE init #-}

--------------------------------------------------------------------------------
-- * Transforming PrimVector
--
-- | /O(n)/ 'map' @f xs@ is the PrimVector obtained by applying @f@ to each
-- element of @xs@.
map :: (Prim a, Prim b) => (a -> b) -> PrimVector a -> PrimVector b
map f = \ (PrimVector ba s l) -> create l (go ba (l+s) s 0)
  where
    go !ba !sl !i !j !mba  | i >= sl = return ()
                           | otherwise = do let x = indexPrimArray ba i
                                            writePrimArray mba j (f x)
                                            go ba sl (i+1) (j+1) mba
{-# INLINE map #-}

-- | /O(n)/ 'reverse' @xs@ efficiently returns the elements of @xs@ in reverse order.
--
reverse :: Prim a => PrimVector a -> PrimVector a
reverse (PrimVector ba s l) = create l (go s)
  where
    !sl = s + l -1
    go !i !mba | i > sl = return ()
               | otherwise = do let x = indexPrimArray ba i
                                writePrimArray mba (sl-i) x
                                go (i+1) mba
{-# INLINE [1] reverse #-}
{-# RULES "reverse/Bytes" [~1] reverse = reverseBytes #-}

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
intersperse :: Prim a => a -> PrimVector a -> PrimVector a
intersperse x v@(PrimVector ba s l)
    | l < 2  = v
    | otherwise = create (2*l-1) (go s 0)
   where
    sl = s + l -1
    go !i !j !mba
        | i == sl = writePrimArray mba j (indexPrimArray ba i)
        | otherwise = do
            writePrimArray mba j (indexPrimArray ba i)
            writePrimArray mba (j+1) (indexPrimArray ba i)
            go (i+1) (j+2) mba
{-# INLINE [1] intersperse #-}
{-# RULES "intersperse/Bytes" [~1] intersperse = intersperseBytes #-}

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
intercalate :: Prim a => PrimVector a -> [PrimVector a] -> PrimVector a
intercalate s = concat . List.intersperse s
{-# INLINE intercalate #-}

-- | /O(n)/ intercalateElem. An efficient way to join [PrimVector]
-- with an element. It's faster than @intercalate (singleton c)@.
--
intercalateElem :: Prim a => a -> [PrimVector a] -> PrimVector a
intercalateElem w = \ vs -> create (len vs) (copy 0 vs)
  where
    len []                  = 0
    len [PrimVector _ _ l]     = l
    len (PrimVector _ _ l:vs) = l + 1 + len vs

    copy !i []                 !mba = return ()
    copy !i (PrimVector ba s l:vs) !mba = do
        let !i' = i + l
        copyPrimArray mba i ba s l
        copy i' vs mba
{-# INLINE intercalateElem #-}

-- | The 'transpose' function transposes the rows and columns of its
-- 'PrimVector' argument.
--
transpose :: Prim a => [PrimVector a] -> [PrimVector a]
transpose vs =
    List.map (packN (List.length vs)) . List.transpose . List.map unpack $ vs
{-# INLINE transpose #-}

--------------------------------------------------------------------------------
--
-- Reducing 'PrimVector's (folds)
--
foldl :: Prim a => (b -> a -> b) -> b -> PrimVector a -> b
foldl f z = List.foldl f z . unpack
{-# INLINE foldl #-}

foldl' :: Prim a => (b -> a -> b) -> b -> PrimVector a -> b
foldl' f z =  \ (PrimVector ba s l) -> go z s (s+l) ba
  where
    -- tail recursive; traverses array left to right
    go !acc !p !q ba | p >= q    = acc
                     | otherwise = go (f acc (indexPrimArray ba p)) (p + 1) q ba
{-# INLINE foldl' #-}

foldl1' :: Prim a => (a -> a -> a) -> a -> PrimVector a -> a
foldl1' f z = \ (PrimVector ba s l) ->
    if l <= 0 then errorEmptyVector "foldl1'"
              else foldl' f (indexPrimArray ba s) (PrimVector ba (s+1) (l-1))
{-# INLINE foldl1' #-}


foldr :: Prim a => (a -> b -> b) -> b -> PrimVector a -> b
foldr f z = List.foldr f z . unpack
{-# INLINE foldr #-}

foldr' :: Prim a => (a -> b -> b) -> b -> PrimVector a -> b
foldr' f z =  \ (PrimVector ba s l) -> go z (s+l-1) s ba
  where
    -- tail recursive; traverses array right to left
    go !acc !p !q ba | p < q     = acc
                     | otherwise = go (f (indexPrimArray ba p) acc) (p - 1) q ba
{-# INLINE foldr' #-}

foldr1' :: Prim a => (a -> a -> a) -> a -> PrimVector a -> a
foldr1' f z = \ (PrimVector ba s l) ->
    if l <= 0 then errorEmptyVector "foldr1'"
              else foldl' f (indexPrimArray ba (s+l-1)) (PrimVector ba s (l-1))
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
concat :: forall a . Prim a => [PrimVector a] -> PrimVector a
concat vs = case pre 0 0 vs of
    (0, _)  -> empty
    (1, _)  -> let Just v = List.find (not . null) vs in v
    (_, l') -> create l' (copy vs l')
  where
    -- pre scan to decide if we really need to copy and calculate total length
    pre :: Int -> Int -> [PrimVector a] -> (Int, Int)
    pre !nacc !lacc [] = (nacc, lacc)
    pre !nacc !lacc (v@(PrimVector _ _ l):vs)
        | l <= 0    = pre nacc lacc vs
        | otherwise = pre (nacc+1) (l+lacc) vs

    copy [] _ _           = return ()
    copy (v:vs) !i !mba = do let PrimVector ba s l = v
                                 !i' = i - l
                             when (l /= 0) (copyPrimArray mba i' ba s l)
                             copy vs i' mba
{-# INLINE concat #-}

-- | Map a function over a 'PrimVector' and concatenate the results
concatMap :: Prim a => (a -> PrimVector a) -> PrimVector a -> PrimVector a
concatMap f = concat . foldr' ((:) . f) []
{-# INLINE concatMap #-}

-- | /O(n)/ Applied to a predicate and a PrimVector, 'any' determines if
-- any element of the 'PrimVector' satisfies the predicate.
any :: Prim a => (a -> Bool) -> PrimVector a -> Bool
any f = List.any f . unpack
{-# INLINE any #-}

-- | /O(n)/ Applied to a predicate and a 'PrimVector', 'all' determines
-- if all elements of the 'PrimVector' satisfy the predicate.
all :: Prim a => (a -> Bool) -> PrimVector a -> Bool
all f = List.all f . unpack
{-# INLINE all #-}

-- | /O(n)/ 'maximum' returns the maximum value from a 'PrimVector'
-- This function will fuse.
-- An exception will be thrown in the case of an empty PrimVector.
maximum :: (Prim a, Ord a) => PrimVector a -> a
maximum = undefined

-- | /O(n)/ 'minimum' returns the minimum value from a 'PrimVector'
-- This function will fuse.
-- An exception will be thrown in the case of an empty PrimVector.
minimum :: (Prim a, Ord a) => PrimVector a -> a
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
scanl :: (Prim a, Prim b) => (b -> a -> b) -> b -> PrimVector a -> PrimVector b
scanl f z = \ (PrimVector ba s l) ->
    create (l+1) (\ mba -> writePrimArray mba 0 z >> go ba z s 1 l mba)
  where
    go !ba !acc !i !j !l !mba
        | j > l = return ()
        | otherwise = do
            let acc' = acc `f` (indexPrimArray ba i)
            writePrimArray mba j acc'
            go ba acc' (i+1) (j+1) l mba
{-# INLINE scanl #-}

-- | 'scanl1' is a variant of 'scanl' that has no starting value argument.
-- This function will fuse.
--
-- > scanl1 f [x1, x2, ...] == [x1, x1 `f` x2, ...]
--
scanl1 :: Prim a => (a -> a -> a) -> PrimVector a -> PrimVector a
scanl1 f = \ (PrimVector ba s l) ->
    if l <= 0 then errorEmptyVector "scanl1"
              else scanl f (indexPrimArray ba s) (PrimVector ba (s+1) (l-1))
{-# INLINE scanl1 #-}

-- | scanr is the right-to-left dual of scanl.
--
scanr :: (Prim a, Prim b) => (a -> b -> b) -> b -> PrimVector a -> PrimVector b
scanr f z = \ (PrimVector ba s l) ->
    create (l+1) (\ mba -> writePrimArray mba l z >> go ba z (s+l-1) (l-1) mba)
  where
    go !ba !acc !i !j !mba
        | j < 0 = return ()
        | otherwise = do
            let acc' = indexPrimArray ba i `f` acc
            writePrimArray mba j acc'
            go ba acc' (i-1) (j-1) mba
{-# INLINE scanr #-}

-- | 'scanr1' is a variant of 'scanr' that has no starting value argument.
scanr1 :: Prim a => (a -> a -> a) -> PrimVector a -> PrimVector a
scanr1 f = \ (PrimVector ba s l) ->
    if l <= 0 then errorEmptyVector "scanl1"
              else scanr f (indexPrimArray ba (s+l-1)) (PrimVector ba s (l-1))
{-# INLINE scanr1 #-}

--------------------------------------------------------------------------------
-- Accumulating maps

-- | The 'mapAccumL' function behaves like a combination of 'map' and
-- 'foldl'; it applies a function to each element of a primitive vector,
-- passing an accumulating parameter from left to right, and returning a
-- final value of this accumulator together with the new list.
--
mapAccumL :: (Prim b, Prim c) => (a -> b -> (a, c)) -> a -> PrimVector b -> (a, PrimVector c)
mapAccumL f z = \ (PrimVector ba s l) -> creating l (go z s 0 (s+l) ba)
  where
    go !acc !i !j !sl !ba !mba
        | i >= sl   = return acc
        | otherwise = do
            let (acc', c) = acc `f` indexPrimArray ba i
            writePrimArray mba j c
            go acc' (i+1) (j+1) sl ba mba
{-# INLINE mapAccumL #-}

-- | The 'mapAccumR' function behaves like a combination of 'map' and
-- 'foldr'; it applies a function to each element of a primitive vector,
-- passing an accumulating parameter from right to left, and returning a
-- final value of this accumulator together with the new primitive vector.
--
mapAccumR :: (Prim b, Prim c) => (a -> b -> (a, c)) -> a -> PrimVector b -> (a, PrimVector c)
mapAccumR f z = \ (PrimVector ba s l) -> creating l (go z (s+l-1) s ba)
  where
    go !acc !i !s !ba !mba
        | i < s     = return acc
        | otherwise = do
            let (acc', c) = acc `f` indexPrimArray ba i
            writePrimArray mba (i-s) c
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
replicate :: (Prim a) => Int -> a -> PrimVector a
replicate n x = create n (\ mba -> setPrimArray mba 0 n x)
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
unfoldr :: Prim b => (a -> Maybe (b, a)) -> a -> PrimVector b
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
unfoldrN :: Prim b => Int -> (a -> Maybe (b, a)) -> a -> (PrimVector b, Maybe a)
unfoldrN n f
    | n < 0     = \ z -> (empty, Just z)
    | otherwise = \ z ->
        let ((r, len), v) = creating n (go z 0)
        in (take len v, r)
  where
    go !acc !i !mba
      | n == i    = return (Just acc, i)
      | otherwise = case f acc of
          Nothing     -> return (Nothing, i)
          Just (x, acc') -> do writePrimArray mba i x
                               go acc' (i+1) mba
{-# INLINE unfoldrN #-}

-- ---------------------------------------------------------------------
-- Substrings

-- | /O(1)/ 'take' @n@, applied to a ByteString @xs@, returns the prefix
-- of @xs@ of length @n@, or @xs@ itself if @n > 'length' xs@.
take :: Prim a => Int -> PrimVector a -> PrimVector a
take n v@(PrimVector ba s l)
    | n <= 0    = empty
    | n >= l    = v
    | otherwise = PrimVector ba s n
{-# INLINE take #-}

-- | /O(1)/ 'drop' @n xs@ returns the suffix of @xs@ after the first @n@
-- elements, or @[]@ if @n > 'length' xs@.
drop :: Prim a => Int -> PrimVector a -> PrimVector a
drop n v@(PrimVector ba s l)
    | n <= 0    = v
    | n >= l    = empty
    | otherwise = PrimVector ba (s+n) (l-n)
{-# INLINE drop #-}

--------------------------------------------------------------------------------
--
-- Unsafe operations
--

unsafeIndex :: Prim a => PrimVector a -> Int -> a
unsafeIndex (PrimVector ba s _) idx = indexPrimArray ba (idx + s)
{-# INLINE unsafeIndex #-}

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

--------------------------------------------------------------------------------

-- Common up near identical calls to `error' to reduce the number
-- constant strings created when compiled:
errorEmptyVector :: String -> a
errorEmptyVector fun = moduleError fun "empty PrimVector"
{-# NOINLINE errorEmptyVector #-}

moduleError :: String -> String -> a
moduleError fun msg = error (moduleErrorMsg fun msg)
{-# NOINLINE moduleError #-}

moduleErrorIO :: String -> String -> IO a
moduleErrorIO fun msg = throwIO . userError $ moduleErrorMsg fun msg
{-# NOINLINE moduleErrorIO #-}

moduleErrorMsg :: String -> String -> String
moduleErrorMsg fun msg = "Data.PrimVector." ++ fun ++ ':':' ':msg

--------------------------------------------------------------------------------
-- FFI
--

foreign import ccall unsafe "bytes.c reverse"
    c_reverse :: MutableByteArray# s -> ByteArray# -> CSize -> IO ()

foreign import ccall unsafe "bytes.c intersperse"
    c_intersperse :: MutableByteArray# s -> ByteArray# -> CSize -> CSize -> CChar -> IO ()

foreign import ccall unsafe "bytes.c _memcmp"
    c_memcmp :: ByteArray# -> CSize -> ByteArray# -> CSize -> CSize -> IO CInt

foreign import capi "Rts.h value LARGE_OBJECT_THRESHOLD"
    rts_LARGE_OBJECT_THRESHOLD :: CInt
