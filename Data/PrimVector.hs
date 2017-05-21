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
  ( -- * The 'PVector' type
    PVector(..)
    -- * Creating 'PVector' and conversion between list
  , createN
  , empty
  , singleton
  , pack, packN, packR, packRN
  , unpack
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
  -- * Transforming PVector
  , map
  , reverse
  , intersperse
  , intercalate
  , intercalateElem
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
data PVector a = PVector
    {-# UNPACK #-} !(PrimArray a) -- payload
    {-# UNPACK #-} !Int         -- offset in elements of type a rather than in bytes
    {-# UNPACK #-} !Int         -- length in elements of type a rather than in bytes
  deriving Typeable


instance {-# OVERLAPPABLE #-} Prim a => Eq (PVector a) where
    (==) = equateBytes
    {-# INLINABLE (==) #-}

equateBytes :: forall a. Prim a => PVector a -> PVector a -> Bool
equateBytes (PVector (PrimArray (ByteArray baA#)) sA lA)
            (PVector (PrimArray (ByteArray baB#)) sB lB) =
    let r = unsafeDupablePerformIO $
            c_memcmp baA# (fromIntegral $ sA * siz)
                     baB# (fromIntegral $ sB * siz) (fromIntegral $ min lA lB * siz)
    in lA == lB && r == 0
  where siz = sizeOf (undefined :: a)

instance {-# OVERLAPPABLE #-} (Prim a, Ord a) => Ord (PVector a) where
    compare = compareVector
    {-# INLINABLE compare #-}

instance {-# OVERLAPPING #-} Ord (PVector Word8) where
    compare = compareBytes
    {-# INLINABLE compare #-}

compareVector :: (Prim a, Ord a) => PVector a -> PVector a -> Ordering
compareVector (PVector baA sA lA) (PVector baB sB lB) = go sA sB
  where
    !slA = sA + lA
    !slB = sB + lB
    go !i !j | i >= slA  = slB `compare` j
             | j >= slB  = slA `compare` i
             | otherwise = let o = indexPrimArray baA i `compare` indexPrimArray baB j
                           in case o of EQ -> go (i+1) (j+1)
                                        x  -> x


compareBytes :: PVector Word8 -> PVector Word8 -> Ordering
compareBytes (PVector (PrimArray (ByteArray baA#)) sA lA)
             (PVector (PrimArray (ByteArray baB#)) sB lB) =
    let r = unsafeDupablePerformIO $
            c_memcmp baA# (fromIntegral sA)
                     baB# (fromIntegral sB) (fromIntegral $ min lA lB)
    in case r `compare` 0 of
        EQ  -> lA `compare` lB
        x  -> x

#if MIN_VERSION_base(4,9,0)
instance Prim a => Semigroup (PVector a) where
    (<>)    = append
#endif

instance Prim a => Monoid (PVector a) where
    mempty  = empty
#if MIN_VERSION_base(4,9,0)
    mappend = (<>)
#else
    mappend = append
#endif
    mconcat = concat

instance NFData (PVector a) where
    rnf PVector{} = ()

instance (Prim a, Show a) => Show (PVector a) where
    showsPrec p ps r = showsPrec p (unpack ps) r

instance (Prim a, Read a) => Read (PVector a) where
    readsPrec p str = [ (pack x, y) | (x, y) <- readsPrec p str ]

instance (Prim a, Data a) => Data (PVector a) where
    gfoldl f z txt = z pack `f` unpack txt
    toConstr _     = error "Data.PVector.toConstr"
    gunfold _ _    = error "Data.PVector.gunfold"
    dataTypeOf _   = mkNoRepType "Data.PVector"

--------------------------------------------------------------------------------
-- Creating 'PVector' and conversion between list
--
-- | Create a 'PVector'.
--
create :: Prim a
       => Int  -- length in elements of type @a@
       -> (forall s. MutablePrimArray s a -> ST s ())  -- initialization function
       -> PVector a
create l fill = runST (do
        mba <- newPrimArray l
        fill mba
        ba <- unsafeFreezePrimArray mba
        return (PVector ba 0 l)
    )
{-# INLINE create #-}

-- | Create a 'PVector' up to a specific length.
--
createN :: Prim a
        => Int  -- length's upper bound
        -> (forall s. MutablePrimArray s a -> ST s Int)  -- initialization function which return the actual length
                                                         -- (must be smaller than upper bound)
        -> PVector a
createN l fill = runST (do
        mba <- newPrimArray l
        l' <- fill mba
        ba <- unsafeFreezePrimArray mba
        assert (l' <= l) $ return (PVector ba 0 l')
    )
{-# INLINE createN #-}

-- | /O(1)/. The empty 'PVector'.
--
empty :: Prim a => PVector a
empty = create 0 (\_ -> return ())

-- | /O(1)/. Single element 'PVector'.
singleton :: Prim a => a -> PVector a
singleton c = create 1 (\ mba -> writePrimArray mba 0 c)
{-# INLINE singleton #-} -- Inline [1] for intercalate rule

-- | /O(n)/ Convert a list into a 'PVector'
--
-- Alias for @'packN' 16@.
--
pack :: Prim a => [a] -> PVector a
pack = packN 16
{-# INLINE pack #-}

-- | /O(n)/ Convert a list into a 'PVector' with an approximate size.
--
-- If the list's length is large than the size given, we simply double the buffer size
-- and continue building.
--
-- This function is a /good consumer/ in the sense of build/foldr fusion.
--
packN :: Prim a => Int -> [a] -> PVector a
packN n0 = \ ws0 -> runST (do mba <- newPrimArray n0
                              SP3 i _ mba' <- foldM go (SP3 0 n0 mba) ws0
                              shrinkMutablePrimArray mba' i
                              ba <- unsafeFreezePrimArray mba'
                              return (PVector ba 0 i)
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
packR :: Prim a => [a] -> PVector a
packR = packRN 16
{-# INLINE packR #-}

-- | /O(n)/ 'packN' in reverse order.
--
-- This function is a /good consumer/ in the sense of build/foldr fusion.
--
packRN :: Prim a => Int -> [a] -> PVector a
packRN n0 = \ ws0 -> runST (do mba <- newPrimArray n0
                               SP3 i n mba' <- foldM go (SP3 (n0-1) n0 mba) ws0
                               ba <- unsafeFreezePrimArray mba'
                               return (PVector ba i (n-i))
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

-- | /O(n)/ Convert 'PVector' to a list.
--
unpack :: Prim a => PVector a -> [a]
unpack (PVector ba s l) = List.map (indexPrimArray ba) [s..s+l-1]
{-# INLINE unpack #-}

--------------------------------------------------------------------------------
-- Basic interface
--
-- |  /O(1)/ The length of a 'PVector'.
--
length :: PVector a -> Int
length (PVector _ _ l) = l
{-# INLINE length #-}

-- | /O(1)/ Test whether a 'PVector' is empty.
null :: PVector a -> Bool
null v = length v == 0
{-# INLINE null #-}

-- | /O()/
append :: Prim a => PVector a -> PVector a -> PVector a
append (PVector _ _  0)  b            = b
append a              (PVector _ _ 0) = a
append (PVector baA sA lA) (PVector baB sB lB) = create (lA+lB) $ \ mba -> do
    copyPrimArray mba 0  baA sA lA
    copyPrimArray mba lA baB sB lB
{-# INLINE append #-}

-- | /O(n)/ 'cons' is analogous to (:) for lists, but of different
-- complexity, as it requires making a copy.
--
cons :: Prim a => a -> PVector a -> PVector a
cons x (PVector ba s l) = create (l+1) $ \ mba -> do
    writePrimArray mba 0 x
    copyPrimArray mba 1 ba s l
{-# INLINE cons #-}

-- | /O(n)/ Append a byte to the end of a 'PVector'
--
snoc :: Prim a => PVector a -> a -> PVector a
snoc (PVector ba s l) x = create (l+1) $ \ mba -> do
    copyPrimArray mba 0 ba s l
    writePrimArray mba l x
{-# INLINE snoc #-}

-- | /O(1)/ Extract the head and tail of a PVector, returning Nothing
-- if it is empty.
--
uncons :: Prim a => PVector a -> Maybe (a, PVector a)
uncons (PVector ba s l)
    | l <= 0    = Nothing
    | otherwise = Just (indexPrimArray ba s, PVector ba (s+1) (l-1))
{-# INLINE uncons #-}

-- | /O(1)/ Extract the 'init' and 'last' of a PVector, returning Nothing
-- if it is empty.
--
unsnoc :: Prim a => PVector a -> Maybe (PVector a, a)
unsnoc (PVector ba s l)
    | l <= 0    = Nothing
    | otherwise = Just (PVector ba s (l-1), indexPrimArray ba (s+l-1))
{-# INLINE unsnoc #-}

-- | /O(1)/ Extract the first element of a PVector, which must be non-empty.
-- An exception will be thrown in the case of an empty PVector.
--
head :: Prim a => PVector a -> a
head (PVector ba s l)
    | l <= 0    = errorEmptyVector "head"
    | otherwise = indexPrimArray ba s
{-# INLINE head #-}

-- | /O(1)/ Extract the elements after the head of a PVector, which must be non-empty.
-- An exception will be thrown in the case of an empty PVector.
tail :: Prim a => PVector a -> PVector a
tail (PVector ba s l)
    | l <= 0    = errorEmptyVector "tail"
    | otherwise = PVector ba (s+1) (l-1)
{-# INLINE tail #-}

-- | /O(1)/ Extract the first element of a PVector, which must be non-empty.
-- An exception will be thrown in the case of an empty PVector.
--
last :: Prim a => PVector a -> a
last (PVector ba s l)
    | l <= 0    = errorEmptyVector "last"
    | otherwise = indexPrimArray ba (s+l-1)
{-# INLINE last #-}

-- | /O(1)/ Extract the elements after the head of a PVector, which must be non-empty.
-- An exception will be thrown in the case of an empty PVector.
init :: Prim a => PVector a -> PVector a
init (PVector ba s l)
    | l <= 0    = errorEmptyVector "init"
    | otherwise = PVector ba s (l-1)
{-# INLINE init #-}

--------------------------------------------------------------------------------
-- * Transforming PVector
--
-- | /O(n)/ 'map' @f xs@ is the PVector obtained by applying @f@ to each
-- element of @xs@.
map :: (Prim a, Prim b) => (a -> b) -> PVector a -> PVector b
map f = \ (PVector ba s l) -> create l (go ba (l+s) s 0)
  where
    go !ba !sl !i !j !mba  | i >= sl = return ()
                           | otherwise = do let x = indexPrimArray ba i
                                            writePrimArray mba j (f x)
                                            go ba sl (i+1) (j+1) mba
{-# INLINE map #-}

-- | /O(n)/ 'reverse' @xs@ efficiently returns the elements of @xs@ in reverse order.
--
reverse :: Prim a => PVector a -> PVector a
reverse (PVector ba s l) = create l (go s)
  where
    !sl = s + l -1
    go !i !mba | i > sl = return ()
               | otherwise = do let x = indexPrimArray ba i
                                writePrimArray mba (sl-i) x
                                go (i+1) mba
{-# INLINE [1] reverse #-}
{-# RULES "reverse/Bytes" [~1] reverse = reverseBytes #-}

reverseBytes :: Bytes -> Bytes
reverseBytes (PVector (PrimArray (ByteArray ba#)) s l) =
    create l (\ (MutablePrimArray (MutableByteArray mba#)) ->
        unsafeIOToST (c_reverse mba# ba# (fromIntegral (s + l))))
{-# INLINE reverseBytes #-}

-- | /O(n)/ The 'intersperse' function takes a 'Word8' and a
-- 'PVector' and \`intersperses\' that byte between the elements of
-- the 'PVector'.  It is analogous to the intersperse function on
-- Lists.
--
intersperse :: Prim a => a -> PVector a -> PVector a
intersperse x v@(PVector ba s l)
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
intersperseBytes w v@(PVector (PrimArray (ByteArray ba#)) s l)
    | l < 2  = v
    | otherwise = create (2*l-1) (\ (MutablePrimArray (MutableByteArray mba#)) ->
            unsafeIOToST
                (c_intersperse mba# ba# (fromIntegral s) (fromIntegral l) (fromIntegral w))
        )
{-# INLINE intersperseBytes #-}

-- | /O(n)/ The 'intercalate' function takes a 'PVector' and a list of
-- 'PVector's and concatenates the list after interspersing the first
-- argument between each element of the list.
--
-- Note: 'intercalate' will force the entire 'PVector' list.
--
intercalate :: Prim a => PVector a -> [PVector a] -> PVector a
intercalate s = concat . List.intersperse s
{-# INLINE intercalate #-}

-- | /O(n)/ intercalateElem. An efficient way to join [PVector]
-- with an element. It's faster than @intercalate (singleton c)@.
--
intercalateElem :: Prim a => a -> [PVector a] -> PVector a
intercalateElem w = \ vs -> create (len vs) (copy 0 vs)
  where
    len []                  = 0
    len [PVector _ _ l]     = l
    len (PVector _ _ l:vs) = l + 1 + len vs

    copy !i []                 !mba = return ()
    copy !i (PVector ba s l:vs) !mba = do let !i' = i + l
                                          copyPrimArray mba i ba s l
                                          copy i' vs mba
{-# INLINE intercalateElem #-}

-- | The 'transpose' function transposes the rows and columns of its
-- 'PVector' argument.
--
transpose :: Prim a => [PVector a] -> [PVector a]
transpose vs =
    List.map (packN (List.length vs)) . List.transpose . List.map unpack $ vs
{-# INLINE transpose #-}

--------------------------------------------------------------------------------
--
-- Special folds
--
-- | /O(n)/ Concatenate a list of ByteStrings.
--
-- Note: 'concat' have to force the entire list to filter out empty 'PVector' and calculate
-- the length for allocation.
--
concat :: Prim a => [PVector a] -> PVector a
concat vs = case pre [] 0 vs of ([], _)    -> empty
                                ([v], _)  -> v
                                (vs', l') -> create l' (copy vs' l')
  where
    -- pre scan to filter empty bytes and calculate total length
    pre :: [PVector a] -> Int -> [PVector a] -> ([PVector a], Int)
    pre vacc !lacc [] = (vacc, lacc)
    pre vacc !lacc (v@(PVector _ _ l):vs)
        | l <= 0    = pre vacc lacc vs
        | otherwise = pre (v:vacc) (l+lacc) vs

    copy [] _ _           = return ()
    copy (v:vs) !i !mba = do let PVector ba s l = v
                                 !i' = i - l
                             copyPrimArray mba i' ba s l
                             copy vs i' mba
{-# INLINE concat #-}

-- | Map a function over a 'PVector' and concatenate the results
concatMap :: Prim a => (a -> PVector a) -> PVector a -> PVector a
concatMap = undefined
{-# INLINE concatMap #-}

-- | /O(n)/ Applied to a predicate and a PVector, 'any' determines if
-- any element of the 'PVector' satisfies the predicate.
any :: (a -> Bool) -> PVector a -> Bool
any = undefined

-- | /O(n)/ Applied to a predicate and a 'PVector', 'all' determines
-- if all elements of the 'PVector' satisfy the predicate.
all :: (a -> Bool) -> PVector a -> Bool
all = undefined

-- | /O(n)/ 'maximum' returns the maximum value from a 'PVector'
-- This function will fuse.
-- An exception will be thrown in the case of an empty PVector.
maximum :: PVector a -> a
maximum = undefined

-- | /O(n)/ 'minimum' returns the minimum value from a 'PVector'
-- This function will fuse.
-- An exception will be thrown in the case of an empty PVector.
minimum :: PVector a -> a
minimum = undefined

--------------------------------------------------------------------------------
--
-- Unsafe operations
--

unsafeIndex :: Prim a => PVector a -> Int -> a
unsafeIndex (PVector ba s _) idx = indexPrimArray ba (idx + s)
{-# INLINE unsafeIndex #-}

--------------------------------------------------------------------------------

-- | 'Bytes' is just primitive word8 vectors.
type Bytes = PVector Word8

-- | Conversion between 'Word8' and 'Char'. Should compile to a no-op.
--
w2c :: Word8 -> Char
w2c (W8# w#) = C# (chr# (word2Int# w#))
{-# INLINE w2c #-}

-- | Unsafe conversion between 'Char' and 'Word8'. This is a no-op and
-- silently truncates to 8 bits Chars > '\255'. It is provided as
-- convenience for PVector construction.
c2w :: Char -> Word8
c2w (C# c#) = W8# (int2Word# (ord# c#))
{-# INLINE c2w #-}

--------------------------------------------------------------------------------

-- Common up near identical calls to `error' to reduce the number
-- constant strings created when compiled:
errorEmptyVector :: String -> a
errorEmptyVector fun = moduleError fun "empty PVector"
{-# NOINLINE errorEmptyVector #-}

moduleError :: String -> String -> a
moduleError fun msg = error (moduleErrorMsg fun msg)
{-# NOINLINE moduleError #-}

moduleErrorIO :: String -> String -> IO a
moduleErrorIO fun msg = throwIO . userError $ moduleErrorMsg fun msg
{-# NOINLINE moduleErrorIO #-}

moduleErrorMsg :: String -> String -> String
moduleErrorMsg fun msg = "Data.PVector." ++ fun ++ ':':' ':msg

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
