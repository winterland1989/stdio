{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}

module Data.Array (
  -- * Arr typeclass
    Arr(..)
  -- * Boxed array type
  , Array(..)
  , MutableArray(..)
  , SmallArray(..)
  , SmallMutableArray(..)
  , uninitialized
  -- * Primitive array type
  , PrimArray(..)
  , MutablePrimArray(..)
  , newPinnedPrimArray, newAlignedPinnedPrimArray
  , primArrayContents, mutablePrimArrayContents
  , isPrimArrayPinned, isMutablePrimArrayPinned
  ) where

import Data.Primitive.Types
import Control.Monad.Primitive
import Data.Primitive.PrimArray
import Data.Primitive.Array
import Data.Primitive.SmallArray
import GHC.ST
import GHC.Prim
import GHC.Types (isTrue#)
import Control.Exception (assert)

uninitialized :: a
uninitialized = error "Data.Array: uninitialized element accessed"

-- | A typeclass to unify box and unboxed array operations.
--
--
class Arr (marr :: * -> * -> *) (arr :: * -> * ) a | arr -> marr, marr -> arr where
    -- | Test
    newArr :: (PrimMonad m, PrimState m ~ s) => Int -> m (marr s a)
    newArrWith :: (PrimMonad m, PrimState m ~ s) => Int -> a -> m (marr s a)

    readArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> m a
    writeArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> a -> m ()
    setArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> Int -> a -> m ()

    indexArr :: arr a -> Int -> a
    indexArrM :: (Monad m) => arr a -> Int -> m a

    freezeArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> Int -> m (arr a)
    thawArr :: (PrimMonad m, PrimState m ~ s) => arr a -> Int -> Int -> m (marr s a)
    unsafeFreezeArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> m (arr a)
    unsafeThawArr :: (PrimMonad m, PrimState m ~ s) => arr a -> m (marr s a)

    copyArr ::  (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> arr a -> Int -> Int -> m ()
    copyMutableArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> marr s a -> Int -> Int -> m ()

    moveArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> marr s a -> Int -> Int -> m ()

    cloneArr :: arr a -> Int -> Int -> arr a
    cloneMutableArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> Int -> m (marr s a)

    resizeMutableArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> m (marr s a)
    shrinkMutableArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> m ()

    sameMutableArr :: marr s a -> marr s a -> Bool
    sizeofArr :: arr a -> Int
    sizeofMutableArr :: marr s a -> Int

instance Arr MutableArray Array a where
    newArr n = assert
        (n>=0)
        (newArray n uninitialized)
    {-# INLINE newArr #-}
    newArrWith n x = assert
        (n>=0)
        (newArray n x)
    {-# INLINE newArrWith #-}
    readArr marr i = assert
        (i>=0 && i<sizeofMutableArray marr)
        (readArray marr i)
    {-# INLINE readArr #-}
    writeArr marr i x = assert
        (i>=0 && i<sizeofMutableArray marr)
        (writeArray marr i x)
    {-# INLINE writeArr #-}
    setArr marr s l x = assert
        (s>=0 && l>=0 && (s+l)<=sizeofMutableArray marr)
        (go s)
      where
        !sl = s + l
        go !i | i >= sl = return ()
              | otherwise = writeArray marr i x >> go (i+1)
    {-# INLINE setArr #-}
    indexArr arr i = assert
        (i>=0 && i<sizeofArray arr)
        (indexArray arr i)
    {-# INLINE indexArr #-}
    indexArrM arr i = assert
        (i>=0 && i<sizeofArray arr)
        (indexArrayM arr i)
    {-# INLINE indexArrM #-}
    freezeArr marr s l = assert
        (s>=0 && l>=0 && (s+l)<=sizeofMutableArray marr)
        (freezeArray marr s l)
    {-# INLINE freezeArr #-}
    thawArr arr s l = assert
        (s>=0 && l>=0 && (s+l)<=sizeofArray arr)
        (thawArray arr s l)
    {-# INLINE thawArr #-}
    unsafeFreezeArr = unsafeFreezeArray
    {-# INLINE unsafeFreezeArr #-}
    unsafeThawArr = unsafeThawArray
    {-# INLINE unsafeThawArr #-}

    copyArr marr s1 arr s2 l  = assert
        (s1>=0 && s2>=0 && l>=0 && (s2+l)<=sizeofArray arr && (s1+l)<=sizeofMutableArray marr)
        (copyArray marr s1 arr s2 l)
    {-# INLINE copyArr #-}
    copyMutableArr marr1 s1 marr2 s2 l = assert
        (s1>=0 && s2>=0 && l>=0 && (s2+l)<=sizeofMutableArray marr2 && (s1+l)<=sizeofMutableArray marr1)
        (copyMutableArray marr1 s1 marr2 s2 l)
    {-# INLINE copyMutableArr #-}

    moveArr marr1 s1 marr2 s2 l
        | l <= 0 = return ()
        | sameMutableArray marr1 marr2 = assert
            (s1>=0 && s2>=0 && l>=0 && (s2+l)<=sizeofMutableArray marr2 && (s1+l)<=sizeofMutableArray marr1)
            (case compare s1 s2 of
                LT ->
                    let !d = s2 - s1
                        !s2l = s2 + l
                        go !i | i >= s2l = return ()
                              | otherwise = do x <- readArray marr2 i
                                               writeArray marr1 (i-d) x
                                               go (i+1)
                    in go s2

                EQ -> return ()

                GT ->
                    let !d = s1 - s2
                        go !i | i < s2 = return ()
                              | otherwise = do x <- readArray marr2 i
                                               writeArray marr1 (i+d) x
                                               go (i-1)
                    in go (s2+l-1)
            )
        | otherwise = copyMutableArray marr1 s1 marr2 s2 l
    {-# INLINE moveArr #-}

    cloneArr arr s l = assert
        (s>=0 && l>=0 && (s+l)<=sizeofArray arr)
        (cloneArray arr s l)
    {-# INLINE cloneArr #-}
    cloneMutableArr marr s l = assert
        (s>=0 && l>=0 && (s+l)<=sizeofMutableArray marr)
        (cloneMutableArray marr s l)
    {-# INLINE cloneMutableArr #-}

    resizeMutableArr marr n = assert
        (n>=sizeofMutableArray marr)
        (do
            marr' <- newArray n uninitialized
            copyMutableArray marr' 0 marr 0 n
            return marr'
        )
    {-# INLINE resizeMutableArr #-}
    shrinkMutableArr _ _ = return ()
    {-# INLINE shrinkMutableArr #-}

    sameMutableArr = sameMutableArray
    {-# INLINE sameMutableArr #-}
    sizeofArr = sizeofArray
    {-# INLINE sizeofArr #-}
    sizeofMutableArr = sizeofMutableArray
    {-# INLINE sizeofMutableArr #-}

instance Arr SmallMutableArray SmallArray a where
    newArr n = assert
        (n>=0)
        (newSmallArray n uninitialized)
    {-# INLINE newArr #-}
    newArrWith n x = assert
        (n>=0)
        (newSmallArray n x)
    {-# INLINE newArrWith #-}
    readArr marr i = assert
        (i>=0 && i<sizeofSmallMutableArray marr)
        (readSmallArray marr i)
    {-# INLINE readArr #-}
    writeArr marr i x = assert
        (i>=0 && i<sizeofSmallMutableArray marr)
        (writeSmallArray marr i x)
    {-# INLINE writeArr #-}
    setArr marr s l x = assert
        (s>=0 && l>=0 && (s+l)<=sizeofSmallMutableArray marr)
        (go s)
      where
        !sl = s + l
        go !i | i >= sl = return ()
              | otherwise = writeSmallArray marr i x >> go (i+1)
    {-# INLINE setArr #-}
    indexArr arr i = assert
        (i>=0 && i<sizeofSmallArray arr)
        (indexSmallArray arr i)
    {-# INLINE indexArr #-}
    indexArrM arr i = assert
        (i>=0 && i<sizeofSmallArray arr)
        (indexSmallArrayM arr i)
    {-# INLINE indexArrM #-}
    freezeArr marr s l = assert
        (s>=0 && l>=0 && (s+l)<=sizeofSmallMutableArray marr)
        (freezeSmallArray marr s l)
    {-# INLINE freezeArr #-}
    thawArr arr s l = assert
        (s>=0 && l>=0 && (s+l)<=sizeofSmallArray arr)
        (thawSmallArray arr s l)
    {-# INLINE thawArr #-}
    unsafeFreezeArr = unsafeFreezeSmallArray
    {-# INLINE unsafeFreezeArr #-}
    unsafeThawArr = unsafeThawSmallArray
    {-# INLINE unsafeThawArr #-}

    copyArr marr s1 arr s2 l  = assert
        (s1>=0 && s2>=0 && l>=0 && (s2+l)<=sizeofSmallArray arr && (s1+l)<=sizeofSmallMutableArray marr)
        (copySmallArray marr s1 arr s2 l)
    {-# INLINE copyArr #-}
    copyMutableArr marr1 s1 marr2 s2 l = assert
        (s1>=0 && s2>=0 && l>=0 && (s2+l)<=sizeofSmallMutableArray marr2 && (s1+l)<=sizeofSmallMutableArray marr1)
        (copySmallMutableArray marr1 s1 marr2 s2 l)
    {-# INLINE copyMutableArr #-}

    moveArr marr1 s1 marr2 s2 l
        | l <= 0 = return ()
        | sameMutableArr marr1 marr2 = assert
            (s1>=0 && s2>=0 && l>=0 && (s2+l)<=sizeofSmallMutableArray marr2 && (s1+l)<=sizeofSmallMutableArray marr1)
            (case compare s1 s2 of
                LT ->
                    let !d = s2 - s1
                        !s2l = s2 + l
                        go !i | i >= s2l = return ()
                              | otherwise = do x <- readSmallArray marr2 i
                                               writeSmallArray marr1 (i-d) x
                                               go (i+1)
                    in go s2

                EQ -> return ()

                GT ->
                    let !d = s1 - s2
                        go !i | i < s2 = return ()
                              | otherwise = do x <- readSmallArray marr2 i
                                               writeSmallArray marr1 (i+d) x
                                               go (i-1)
                    in go (s2+l-1)
            )
        | otherwise = copySmallMutableArray marr1 s1 marr2 s2 l
    {-# INLINE moveArr #-}

    cloneArr arr s l = assert
        (s>=0 && l>=0 && (s+l)<=sizeofSmallArray arr)
        (cloneSmallArray arr s l)
    {-# INLINE cloneArr #-}
    cloneMutableArr marr s l = assert
        (s>=0 && l>=0 && (s+l)<=sizeofSmallMutableArray marr)
        (cloneSmallMutableArray marr s l)
    {-# INLINE cloneMutableArr #-}

    resizeMutableArr marr n = assert
        (n>=sizeofSmallMutableArray marr)
        (do
            marr' <- newSmallArray n uninitialized
            copySmallMutableArray marr' 0 marr 0 n
            return marr'
        )
    {-# INLINE resizeMutableArr #-}
    shrinkMutableArr _ _ = return ()
    {-# INLINE shrinkMutableArr #-}

    sameMutableArr (SmallMutableArray smarr1#) (SmallMutableArray smarr2#) =
        isTrue# (sameSmallMutableArray# smarr1# smarr2#)
    {-# INLINE sameMutableArr #-}
    sizeofArr = sizeofSmallArray
    {-# INLINE sizeofArr #-}
    sizeofMutableArr = sizeofSmallMutableArray
    {-# INLINE sizeofMutableArr #-}

instance Prim a => Arr MutablePrimArray PrimArray a where
    newArr n = assert
        (n>=0)
        (newPrimArray n)
    {-# INLINE newArr #-}
    newArrWith n x = assert
        (n>=0)
        (do marr <- newPrimArray n
            setPrimArray marr 0 n x
            return marr
        )
    {-# INLINE newArrWith #-}
    readArr marr i = assert
        (i>=0 && i<sizeofMutablePrimArray marr)
        (readPrimArray marr i)
    {-# INLINE readArr #-}
    writeArr marr i x = assert
        (i>=0 && i<sizeofMutablePrimArray marr)
        (writePrimArray marr i x)
    {-# INLINE writeArr #-}
    setArr marr s l x = assert
        (s>=0 && l>=0 && (s+l)<=sizeofMutablePrimArray marr)
        (setPrimArray marr s l x)
    {-# INLINE setArr #-}
    indexArr arr i = assert
        (i>=0 && i<sizeofPrimArray arr)
        (indexPrimArray arr i)
    {-# INLINE indexArr #-}
    indexArrM arr i = assert
        (i>=0 && i<sizeofPrimArray arr)
        (return (indexPrimArray arr i))
    {-# INLINE indexArrM #-}
    freezeArr marr s l = assert
        (s>=0 && l>=0 && (s+l)<=sizeofMutablePrimArray marr)
        (do marr' <- newPrimArray l
            copyMutablePrimArray marr' 0 marr s l
            unsafeFreezePrimArray marr'
        )
    {-# INLINE freezeArr #-}
    thawArr arr s l = assert
        (s>=0 && l>=0 && (s+l)<=sizeofPrimArray arr)
        (do marr' <- newPrimArray l
            copyPrimArray marr' 0 arr s l
            return marr'
        )
    {-# INLINE thawArr #-}
    unsafeFreezeArr = unsafeFreezePrimArray
    {-# INLINE unsafeFreezeArr #-}
    unsafeThawArr = unsafeThawPrimArray
    {-# INLINE unsafeThawArr #-}

    copyArr marr s1 arr s2 l = assert
        (s1>=0 && s2>=0 && l>=0 && (s2+l)<=sizeofPrimArray arr && (s1+l)<=sizeofMutablePrimArray marr)
        (copyPrimArray marr s1 arr s2 l)
    {-# INLINE copyArr #-}
    copyMutableArr marr1 s1 marr2 s2 l = assert
        (s1>=0 && s2>=0 && l>=0 && (s2+l)<=sizeofMutablePrimArray marr1 && (s1+l)<=sizeofMutablePrimArray marr2)
        (copyMutablePrimArray marr1 s1 marr2 s2 l)
    {-# INLINE copyMutableArr #-}

    moveArr marr1 s1 marr2 s2 l = assert
        (s1>=0 && s2>=0 && l>=0 && (s2+l)<=sizeofMutablePrimArray marr1 && (s1+l)<=sizeofMutablePrimArray marr2)
        (movePrimArray marr1 s1 marr2 s2 l)
    {-# INLINE moveArr #-}

    cloneArr arr s l = assert
        (s>=0 && l>=0 && s+l<=sizeofPrimArray arr)
        (runST (do
            marr <- newPrimArray l
            copyPrimArray marr 0 arr s l
            unsafeFreezePrimArray marr
        ))
    {-# INLINE cloneArr #-}
    cloneMutableArr marr s l = assert
        (s>=0 && l>=0 && s+l<=sizeofMutablePrimArray marr)
        (do marr' <- newPrimArray l
            copyMutablePrimArray marr' 0 marr s l
            return marr'
        )
    {-# INLINE cloneMutableArr #-}

    resizeMutableArr marr n = assert
        (n>=sizeofMutablePrimArray marr)
        (resizeMutablePrimArray marr n)
    {-# INLINE resizeMutableArr #-}
    shrinkMutableArr marr n = assert
        (n<=sizeofMutablePrimArray marr)
        (shrinkMutablePrimArray marr n)
    {-# INLINE shrinkMutableArr #-}

    sameMutableArr = sameMutablePrimArray
    {-# INLINE sameMutableArr #-}
    sizeofArr = sizeofPrimArray
    {-# INLINE sizeofArr #-}
    sizeofMutableArr = sizeofMutablePrimArray
    {-# INLINE sizeofMutableArr #-}
