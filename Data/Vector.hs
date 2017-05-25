{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Data.Vector where

import GHC.Exts (IsList(..))
import GHC.ST
import Data.Primitive.Types
import Data.Array
import qualified Data.PrimVector as P
import Data.Word
import Data.Bits (shiftL)
import Data.List as List
import Data.Foldable (foldlM, foldrM)
import GHC.Types (SPEC(..))

class (Arr (MArray v) (IArray v) a) => Vect v a where
    type MArray v :: * -> * -> *
    type IArray v :: * -> *

    toArr :: v a -> (IArray v a, Int, Int)
    fromArr :: IArray v a -> Int -> Int -> v a

instance Prim a => Vect P.PrimVector a where
    type MArray P.PrimVector = MutablePrimArray
    type IArray P.PrimVector = PrimArray

    toArr (P.PrimVector arr s l) = (arr, s, l)
    {-# INLINE toArr #-}
    fromArr arr s l = P.PrimVector arr s l
    {-# INLINE fromArr #-}

-- | /O(n)/ Convert a list into a 'PrimVector'
--
-- Alias for @'packN' 16@.
--
pack :: Vect v a => [a] -> v a
pack = packN 16
{-# INLINE pack #-}

-- | /O(n)/ Convert a list into a 'PrimVector' with an approximate size.
--
-- If the list's length is large than the size given, we simply double the buffer size
-- and continue building.
--
-- This function is a /good consumer/ in the sense of build/foldr fusion.
--
packN :: forall v a. Vect v a => Int -> [a] -> v a
packN n0 = \ ws0 -> runST (do mba <- newArr n0
                              (SP3 i _ mba') <- foldlM go (SP3 0 n0 mba) ws0
                              shrinkMutableArr mba' i
                              ba <- unsafeFreezeArr mba'
                              return (fromArr ba 0 i)
                          )
  where
    -- It's critical that this function get specialized and unboxed
    -- Keep an eye on its core!
    go :: (SP3 (MArray v s a)) -> a -> ST s (SP3 (MArray v s a))
    go (SP3 i n mba) !x =
        if i < n
        then do writeArr mba i x
                let !i' = i+1
                return (SP3 i' n mba)
        else do let !n' = n `shiftL` 1
                    !i' = i+1
                !mba' <- resizeMutableArr mba n'
                writeArr mba' i x
                return (SP3 i' n' mba')
{-# INLINE packN #-}

-- | /O(n)/
--
-- Alias for @'packRN' 16@.
--
packR :: Vect v a => [a] -> v a
packR = packRN 16
{-# INLINE packR #-}

-- | /O(n)/ 'packN' in reverse order.
--
-- This function is a /good consumer/ in the sense of build/foldr fusion.
--
packRN :: forall v a. Vect v a => Int -> [a] -> v a
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

-- | /O(n)/ Convert 'PrimVector' to a list.
--
-- Unpacking is done lazily. i.e. we will retain reference to the array until all element are consumed.
-- This function is a /good producer/ in the sense of build/foldr fusion.
--
unpack :: Vect v a => v a -> [a]
unpack v = let (ba, s, l) = toArr v in List.map (indexArr ba) [s..s+l-1]
{-# INLINE unpack #-}

map :: forall u a v b. (Vect u a, Vect v b) => (a -> b) -> u a -> v b
map f = \ v -> let (ba, s, l) = toArr v in create l (go ba (l+s) s 0)
  where
    go :: (IArray u a) -> Int -> Int -> Int -> (MArray v s b) -> ST s ()
    go !ba !sl !i !j !mba  | i >= sl = return ()
                           | otherwise = do x <- indexArrM ba i
                                            writeArr mba j (f x)
                                            go ba sl (i+1) (j+1) mba
{-# INLINE map #-}

foldl :: (Vect v a) => (b -> a -> b) -> b -> v a -> b
foldl f z = List.foldl f z . unpack
{-# INLINE foldl #-}

foldl' :: (Vect v a) => (b -> a -> b) -> b -> v a -> b
foldl' f z =  \ v -> let (ba, s, l) = toArr v in go z s (s+l) ba
  where
    -- tail recursive; traverses array left to right
    go !acc !p !q ba | p >= q    = acc
                     | otherwise = go (f acc (indexArr ba p)) (p + 1) q ba
{-# INLINE foldl' #-}

create :: Vect v a
       => Int  -- length in elements of type @a@
       -> (forall s. MArray v s a -> ST s ())  -- initialization function
       -> v a
create l fill = runST (do
        mba <- newArr l
        fill mba
        ba <- unsafeFreezeArr mba
        return (fromArr ba 0 l)
    )
{-# INLINE create #-}

