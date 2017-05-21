{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- |
--
--
--
--
--

module Data.Vector
  ( -- * The 'Vector' type
    Vector(..)
    -- * Creating 'Vector' and conversion between list
  , create
  , createN
  , createWith
  , createWithN
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

  -- * Reducing 'ByteString's (folds)
  , foldl
  , foldl'
  , foldl1
  , foldl1'

  -- * Transforming Vector
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
import Data.Primitive.Array
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
--
--
data Vector a = Vector {-# UNPACK #-} !(Array a) -- payload
                       {-# UNPACK #-} !Int       -- offset
                       {-# UNPACK #-} !Int       -- length
    deriving Typeable

instance Eq a => Eq (Vector a) where
    (==) = equateVector
    {-# INLINABLE (==) #-}

equateVector :: Eq a => Vector a -> Vector a -> Bool
equateVector (Vector aA sA lA) (Vector aB sB lB) = lA == lB && go sA sB
  where
    !slA = sA + lA
    go !i !j | i >= slA  = True  -- lA == lB must hold
             | otherwise = (indexArray aA i == indexArray aB j) && go (i+1) (j+1)

instance Ord a => Ord (Vector a) where
    compare = compareVector
    {-# INLINABLE compare #-}

compareVector :: Ord a => Vector a -> Vector a -> Ordering
compareVector (Vector aA sA lA) (Vector aB sB lB) = go sA sB
  where
    !slA = sA + lA
    !slB = sB + lB
    go !i !j | i >= slA  = slB `compare` j
             | j >= slB  = slA `compare` i
             | otherwise = let o = indexArray aA i `compare` indexArray aB j
                           in case o of EQ -> go (i+1) (j+1)
                                        x  -> x

#if MIN_VERSION_base(4,9,0)
instance Semigroup (Vector a) where
    (<>)    = append
#endif

instance Monoid (Vector a) where
    mempty  = empty
#if MIN_VERSION_base(4,9,0)
    mappend = (<>)
#else
    mappend = append
#endif
    mconcat = concat

instance NFData a => NFData (Vector a) where
    rnf (Vector arr s l) = go s
      where
        !sl = s + l
        go !i | i >= sl   = ()
              | otherwise = indexArray arr i `seq` go (i+1)

instance Show a => Show (Vector a) where
    showsPrec p ps r = showsPrec p (unpack ps) r

instance Read a => Read (Vector a) where
    readsPrec p str = [ (pack (read x), y) | (x, y) <- readsPrec p str ]

instance Data a => Data (Vector a) where
    gfoldl f z v   = z pack `f` unpack v
    toConstr _     = error "Data.Vector.toConstr"
    gunfold _ _    = error "Data.Vector.gunfold"
    dataTypeOf _   = mkNoRepType "Data.Vector"

--------------------------------------------------------------------------------
-- Creating 'Vector' and conversion between list

uninitialized :: a
uninitialized = error "Data.Vector:vector element uninitialized"

-- | Create a 'Vector'.
--
create :: Int
       -> (forall s. MutableArray s a -> ST s ())  -- initialization function
       -> Vector a
create l = createWith l uninitialized
{-# INLINE create #-}

createN :: Int
        -> (forall s. MutableArray s a -> ST s Int)  -- initialization function
        -> Vector a
createN l = createWithN l uninitialized
{-# INLINE createN #-}

createWith :: Int                                      -- length's upper bound
           -> a
           -> (forall s. MutableArray s a -> ST s ())  -- initialization function
           -> Vector a
createWith l def fill = runST (do
        marr <- newArray l def
        fill marr
        arr <- unsafeFreezeArray marr
        return (Vector arr 0 l)
    )
{-# INLINE createWith #-}

-- | Create a 'Vector' up to a specific length.
--
createWithN :: Int                                       -- length's upper bound
            -> a
            -> (forall s. MutableArray s a -> ST s Int)  -- initialization function which return the actual length
                                                         -- (must be smaller than upper bound)
            -> Vector a
createWithN l def fill = runST (do
        marr <- newArray l def
        l' <- fill marr
        arr <- unsafeFreezeArray marr
        assert (l' <= l) $ return (Vector arr 0 l')
    )
{-# INLINE createWithN #-}

-- | /O(1)/. The empty 'Vector'.
--
empty :: Vector a
empty = create 0 (\_ -> return ())

-- | /O(1)/ Convert a 'a' into a 'Vector'
singleton :: a -> Vector a
singleton c = create 1 (\ marr -> writeArray marr 0 c)
{-# INLINE [1] singleton #-} -- Inline [1] for intercalate rule


-- | /O(n)/ Convert a list into a 'Vector'
--
pack :: [a] -> Vector a
pack = packN 32
{-# INLINE pack #-}


-- | /O(n)/ Convert a list into a 'Vector' with an approximate size.
--
-- If the list's length is large than the size given, we simply double the buffer size
-- and continue building(using re-allocate and copy).
-- This function consume list lazily, and run faster than 'pack''
-- (even the initial size is many times smaller).
--

packN :: Int -> [a] -> Vector a
packN n0 ws0 = runST (do marr <- newArray n0 uninitialized
                         SP3 i _ marr' <- foldM go (SP3 0 n0 marr) ws0
                         arr <- unsafeFreezeArray marr'
                         return (Vector arr 0 i)
                     )
  where
    go :: SP3 s a -> a -> ST s (SP3 s a)
    go (SP3 i n marr) x = if i < n then do writeArray marr i x
                                           return (SP3 (i+1) n marr)
                                   else do let !n' = n*2
                                           marr' <- newArray n uninitialized
                                           copyMutableArray marr' 0 marr 0 n'
                                           writeArray marr' i x
                                           return (SP3 (i+1) n' marr')

data SP3 s a = SP3 {-# UNPACK #-}!Int
                   {-# UNPACK #-}!Int
                   {-# UNPACK #-}!(MutableArray s a)
{-# INLINE packN #-}

-- | /O(n)/ Convert a list into a 'Vector'
--
-- NOTE. This function have to force the entire list to found out the length for allocation.
-- Most of the time you should use 'packN' or 'pack' instead.
--
pack' :: [a] -> Vector a
pack' ws0 = create (List.length ws0) (go 0 ws0)
  where
    go !i []     !_   = return ()
    go !i (w:ws) !marr = writeArray marr i w >> go (i+1) ws marr
{-# INLINE pack' #-}

-- | /O(n)/ Convert 'Vector' to a 'a' list.
--
unpack :: Vector a-> [a]
unpack (Vector arr s l) = List.map (indexArray arr) [s..s+l-1]
{-# INLINE unpack #-}

--------------------------------------------------------------------------------
-- Basic interface
--
-- |  /O(1)/ The length of a 'Vector'.
--
length :: Vector a -> Int
length (Vector _ _ l) = l
{-# INLINE length #-}

-- | /O(1)/ Test whether a 'Vector' is empty.
null :: Vector a -> Bool
null v = length v == 0
{-# INLINE null #-}

-- | /O()/
append :: Vector a -> Vector a -> Vector a
append (Vector _ _  0)  b            = b
append a              (Vector _ _ 0) = a
append (Vector arrA sA lA) (Vector arrB sB lB) = create (lA+lB) $ \ marr -> do
    copyArray marr 0  arrA sA lA
    copyArray marr lA arrB sB lB
{-# INLINE append #-}

-- | /O(n)/ 'cons' is analogous to (:) for lists, but of different
-- complexity, as it requires making a copy.
cons :: a -> Vector a -> Vector a
cons w (Vector arr s l) = create (l+1) $ \ marr -> do
    writeArray marr 0 w
    copyArray marr 1 arr s l
{-# INLINE cons #-}

-- | /O(n)/ Append a byte to the end of a 'Vector'
snoc :: Vector a -> a -> Vector a
snoc (Vector arr s l) w = create (l+1) $ \ marr -> do
    copyArray marr 0 arr s l
    writeArray marr l w
{-# INLINE snoc #-}

-- | /O(1)/ Extract the head and tail of a Vector, returning Nothing
-- if it is empty.
--
uncons :: Vector a -> Maybe (a, Vector a)
uncons (Vector arr s l)
    | l <= 0    = Nothing
    | otherwise = Just (indexArray arr s, Vector arr (s+1) (l-1))
{-# INLINE uncons #-}

-- | /O(1)/ Extract the 'init' and 'last' of a Vector, returning Nothing
-- if it is empty.
--
unsnoc :: Vector a -> Maybe (Vector a, a)
unsnoc (Vector arr s l)
    | l <= 0    = Nothing
    | otherwise = Just (Vector arr s (l-1), indexArray arr (s+l-1))
{-# INLINE unsnoc #-}

-- | /O(1)/ Extract the first element of a Vector, which must be non-empty.
-- An exception will be thrown in the case of an empty Vector.
--
head :: Vector a -> a
head (Vector arr s l)
    | l <= 0    = errorEmptyVector "head"
    | otherwise = indexArray arr s
{-# INLINE head #-}

-- | /O(1)/ Extract the elements after the head of a Vector, which must be non-empty.
-- An exception will be thrown in the case of an empty Vector.
tail :: Vector a -> Vector a
tail (Vector arr s l)
    | l <= 0    = errorEmptyVector "tail"
    | otherwise = Vector arr (s+1) (l-1)
{-# INLINE tail #-}

-- | /O(1)/ Extract the first element of a Vector, which must be non-empty.
-- An exception will be thrown in the case of an empty Vector.
--
last :: Vector a -> a
last (Vector arr s l)
    | l <= 0    = errorEmptyVector "last"
    | otherwise = indexArray arr (s+l+1)
{-# INLINE last #-}

-- | /O(1)/ Extract the elements after the head of a Vector, which must be non-empty.
-- An exception will be thrown in the case of an empty Vector.
init :: Vector a -> Vector a
init (Vector arr s l)
    | l <= 0    = errorEmptyVector "init"
    | otherwise = Vector arr s (l-1)
{-# INLINE init #-}

--------------------------------------------------------------------------------
--
-- Reducing 'PVector's (folds)
--

--------------------------------------------------------------------------------
-- * Transforming Vector
--
-- | /O(n)/ 'map' @f xs@ is the Vector obtained by applying @f@ to each
-- element of @xs@.
map :: (a -> a) -> Vector a -> Vector a
map f = \ (Vector arr s l) -> create l (go arr (l+s) s)
  where
    go arr !sl !i !marr | i >= sl = return ()
                        | otherwise = do let x = indexArray arr i
                                         writeArray marr i (f x)
                                         go arr sl (i+1) marr
{-# INLINE map #-}

-- | /O(n)/ 'reverse' @xs@ efficiently returns the elements of @xs@ in reverse order.
reverse :: Vector a -> Vector a
reverse (Vector arr s l) = create l (go s)
  where
    !sl = s + l - 1
    go !i !marr | i > sl = return ()
                | otherwise = do let x = indexArray arr i
                                 writeArray marr (sl - i) x
                                 go (i+1) marr
{-# INLINE reverse #-}

-- | /O(n)/ The 'intersperse' function takes a 'a' and a
-- 'Vector' and \`intersperses\' that byte between the elements of
-- the 'Vector'.  It is analogous to the intersperse function on
-- Lists.
intersperse :: a -> Vector a -> Vector a
intersperse x v@(Vector arr s l)
    | l < 2  = v
    | otherwise = create (2*l-1) (go s 0)
   where
    sl = s + l -1
    go !i !j !marr | i == sl   = writeArray marr j (indexArray arr i)
                   | otherwise = do writeArray marr j (indexArray arr i)
                                    writeArray marr (j+1) (indexArray arr i)
                                    go (i+1) (j+2) marr
{-# INLINE intersperse #-}

-- | /O(n)/ The 'intercalate' function takes a 'Vector' and a list of
-- 'Vector's and concatenates the list after interspersing the first
-- argument between each element of the list.
intercalate :: Vector a -> [Vector a] -> Vector a
intercalate s = concat . List.intersperse s
{-# INLINE [1] intercalate #-}

-- | The 'transpose' function transposes the rows and columns of its
-- 'Vector' argument.
--
transpose :: [Vector a] -> [Vector a]
transpose vs =
    List.map (packN (List.length vs)) . List.transpose . List.map unpack $ vs
{-# INLINE transpose #-}

--------------------------------------------------------------------------------
--
-- Special folds
--
-- | /O(n)/ Concatenate a list of Strings.
--
-- Note: 'concat' have to force the entire list to filter out empty 'Vector' and calculate
-- the length for allocation.
--
concat :: [Vector a] -> Vector a
concat vs = case pre [] 0 vs of ([], _)    -> empty
                                ([v], _)  -> v
                                (vs', l') -> create l' (copy vs' l')
  where
    -- pre scan to filter empty bytes and calculate total length
    pre :: [Vector a] -> Int -> [Vector a] -> ([Vector a], Int)
    pre vacc !lacc [] = (vacc, lacc)
    pre vacc !lacc (v@(Vector _ _ l):vs)
        | l <= 0    = pre vacc lacc vs
        | otherwise = pre (v:vacc) (l+lacc) vs

    copy [] _ marr     = return ()
    copy (v:vs) !i !marr = do let Vector arr s l = v
                                  i' = i - l
                              copyArray marr i' arr s l
                              copy vs i' marr

-- | Map a function over a 'Vector' and concatenate the results
concatMap :: (a -> Vector a) -> Vector a -> Vector a
concatMap = undefined

-- | /O(n)/ Applied to a predicate and a Vector, 'any' determines if
-- any element of the 'Vector' satisfies the predicate.
any :: (a -> Bool) -> Vector a -> Bool
any = undefined

-- | /O(n)/ Applied to a predicate and a 'Vector', 'all' determines
-- if all elements of the 'Vector' satisfy the predicate.
all :: (a -> Bool) -> Vector a -> Bool
all = undefined

-- | /O(n)/ 'maximum' returns the maximum value from a 'Vector'
-- This function will fuse.
-- An exception will be thrown in the case of an empty Vector.
maximum :: Vector a -> a
maximum = undefined

-- | /O(n)/ 'minimum' returns the minimum value from a 'Vector'
-- This function will fuse.
-- An exception will be thrown in the case of an empty Vector.
minimum :: Vector a -> a
minimum = undefined

--------------------------------------------------------------------------------
--
-- Unsafe operations
--

unsafeIndex :: Vector a -> Int -> a
unsafeIndex (Vector arr s _) idx = indexArray arr (idx + s)
{-# INLINE unsafeIndex #-}

--------------------------------------------------------------------------------

-- Common up near identical calls to `error' to reduce the number
-- constant strings created when compiled:
errorEmptyVector :: String -> a
errorEmptyVector fun = moduleError fun "empty Vector"
{-# NOINLINE errorEmptyVector #-}

moduleError :: String -> String -> a
moduleError fun msg = error (moduleErrorMsg fun msg)
{-# NOINLINE moduleError #-}

moduleErrorIO :: String -> String -> IO a
moduleErrorIO fun msg = throwIO . userError $ moduleErrorMsg fun msg
{-# NOINLINE moduleErrorIO #-}

moduleErrorMsg :: String -> String -> String
moduleErrorMsg fun msg = "Data.Vector." ++ fun ++ ':':' ':msg
