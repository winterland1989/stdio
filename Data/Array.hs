{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}

-- | Unified unboxed and boxed array operations using functional dependencies.
--
-- All operations are NOT bound checked, if you need checked operations please use "Data.Array.Checked".
-- It exports exactly same APIs so that you can switch between without pain.
--
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
  , copyPrimArrayToPtr, copyMutablePrimArrayToPtr, copyMutablePrimArrayFromPtr
  -- * The 'ArrayException' type
  , ArrayException(..)
  ) where

import Data.Primitive.Types
import Control.Monad.Primitive
import Control.Exception (ArrayException(..), throw)
import Data.Primitive.PrimArray
import Data.Primitive.Array
import Data.Primitive.SmallArray
import GHC.ST
import GHC.Prim
import GHC.Types (isTrue#)

-- | Bottom value (@throw ('UndefinedElement' "Data.Array.uninitialized")@)
-- for initialize new boxed array('Array', 'SmallArray'..).
--
uninitialized :: a
uninitialized = throw (UndefinedElement "Data.Array.uninitialized")

-- | A typeclass to unify box & unboxed, mutable & immutable array operations.
--
-- Most of these functions simply wrap their primitive counterpart, if there's no primitive ones,
-- we polyfilled using other operations to get the same semantics.
--
-- One exception is that 'shrinkMutableArr' only perform closure resizing on 'PrimArray' because
-- current RTS support only that, 'shrinkMutableArr' will do nothing on other array type.
--
-- It's reasonable to trust GHC with specializing & inlining these polymorphric functions.
-- They are used across this package and perform identical to their monomophric counterpart.
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
    sizeofMutableArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> m Int

instance Arr MutableArray Array a where
    newArr n = newArray n uninitialized
    {-# INLINE newArr #-}
    newArrWith n x = newArray n x
    {-# INLINE newArrWith #-}
    readArr marr i = readArray marr i
    {-# INLINE readArr #-}
    writeArr marr i x = writeArray marr i x
    {-# INLINE writeArr #-}
    setArr marr s l x = go s
      where
        !sl = s + l
        go !i | i >= sl = return ()
              | otherwise = writeArray marr i x >> go (i+1)
    {-# INLINE setArr #-}
    indexArr arr i = indexArray arr i
    {-# INLINE indexArr #-}
    indexArrM arr i = indexArrayM arr i
    {-# INLINE indexArrM #-}
    freezeArr marr s l = freezeArray marr s l
    {-# INLINE freezeArr #-}
    thawArr arr s l = thawArray arr s l
    {-# INLINE thawArr #-}
    unsafeFreezeArr = unsafeFreezeArray
    {-# INLINE unsafeFreezeArr #-}
    unsafeThawArr = unsafeThawArray
    {-# INLINE unsafeThawArr #-}

    copyArr marr s1 arr s2 l  = copyArray marr s1 arr s2 l
    {-# INLINE copyArr #-}
    copyMutableArr marr1 s1 marr2 s2 l = copyMutableArray marr1 s1 marr2 s2 l
    {-# INLINE copyMutableArr #-}

    moveArr marr1 s1 marr2 s2 l
        | l <= 0 = return ()
        | sameMutableArray marr1 marr2 =
            case compare s1 s2 of
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
        | otherwise = copyMutableArray marr1 s1 marr2 s2 l
    {-# INLINE moveArr #-}

    cloneArr arr s l = cloneArray arr s l
    {-# INLINE cloneArr #-}
    cloneMutableArr marr s l = cloneMutableArray marr s l
    {-# INLINE cloneMutableArr #-}

    resizeMutableArr marr n = do
        marr' <- newArray n uninitialized
        copyMutableArray marr' 0 marr 0 n
        return marr'
    {-# INLINE resizeMutableArr #-}
    shrinkMutableArr _ _ = return ()
    {-# INLINE shrinkMutableArr #-}

    sameMutableArr = sameMutableArray
    {-# INLINE sameMutableArr #-}
    sizeofArr = sizeofArray
    {-# INLINE sizeofArr #-}
    sizeofMutableArr = return . sizeofMutableArray
    {-# INLINE sizeofMutableArr #-}

instance Arr SmallMutableArray SmallArray a where
    newArr n = newSmallArray n uninitialized
    {-# INLINE newArr #-}
    newArrWith n x = newSmallArray n x
    {-# INLINE newArrWith #-}
    readArr marr i = readSmallArray marr i
    {-# INLINE readArr #-}
    writeArr marr i x = writeSmallArray marr i x
    {-# INLINE writeArr #-}
    setArr marr s l x = go s
      where
        !sl = s + l
        go !i | i >= sl = return ()
              | otherwise = writeSmallArray marr i x >> go (i+1)
    {-# INLINE setArr #-}
    indexArr arr i = indexSmallArray arr i
    {-# INLINE indexArr #-}
    indexArrM arr i = indexSmallArrayM arr i
    {-# INLINE indexArrM #-}
    freezeArr marr s l = freezeSmallArray marr s l
    {-# INLINE freezeArr #-}
    thawArr arr s l = thawSmallArray arr s l
    {-# INLINE thawArr #-}
    unsafeFreezeArr = unsafeFreezeSmallArray
    {-# INLINE unsafeFreezeArr #-}
    unsafeThawArr = unsafeThawSmallArray
    {-# INLINE unsafeThawArr #-}

    copyArr marr s1 arr s2 l  = copySmallArray marr s1 arr s2 l
    {-# INLINE copyArr #-}
    copyMutableArr marr1 s1 marr2 s2 l = copySmallMutableArray marr1 s1 marr2 s2 l
    {-# INLINE copyMutableArr #-}

    moveArr marr1 s1 marr2 s2 l
        | l <= 0 = return ()
        | sameMutableArr marr1 marr2 =
            case compare s1 s2 of
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
        | otherwise = copySmallMutableArray marr1 s1 marr2 s2 l
    {-# INLINE moveArr #-}

    cloneArr arr s l = cloneSmallArray arr s l
    {-# INLINE cloneArr #-}
    cloneMutableArr marr s l = cloneSmallMutableArray marr s l
    {-# INLINE cloneMutableArr #-}

    resizeMutableArr marr n = do
        marr' <- newSmallArray n uninitialized
        copySmallMutableArray marr' 0 marr 0 n
        return marr'
    {-# INLINE resizeMutableArr #-}
    shrinkMutableArr _ _ = return ()
    {-# INLINE shrinkMutableArr #-}

    sameMutableArr (SmallMutableArray smarr1#) (SmallMutableArray smarr2#) =
        isTrue# (sameSmallMutableArray# smarr1# smarr2#)
    {-# INLINE sameMutableArr #-}
    sizeofArr = sizeofSmallArray
    {-# INLINE sizeofArr #-}
    sizeofMutableArr = return . sizeofSmallMutableArray
    {-# INLINE sizeofMutableArr #-}

instance Prim a => Arr MutablePrimArray PrimArray a where
    newArr n = newPrimArray n
    {-# INLINE newArr #-}
    newArrWith n x = do
        marr <- newPrimArray n
        setPrimArray marr 0 n x
        return marr
    {-# INLINE newArrWith #-}
    readArr marr i = readPrimArray marr i
    {-# INLINE readArr #-}
    writeArr marr i x = writePrimArray marr i x
    {-# INLINE writeArr #-}
    setArr marr s l x = setPrimArray marr s l x
    {-# INLINE setArr #-}
    indexArr arr i = indexPrimArray arr i
    {-# INLINE indexArr #-}
    indexArrM arr i = return (indexPrimArray arr i)
    {-# INLINE indexArrM #-}
    freezeArr marr s l = do
        marr' <- newPrimArray l
        copyMutablePrimArray marr' 0 marr s l
        unsafeFreezePrimArray marr'
    {-# INLINE freezeArr #-}
    thawArr arr s l = do
        marr' <- newPrimArray l
        copyPrimArray marr' 0 arr s l
        return marr'
    {-# INLINE thawArr #-}
    unsafeFreezeArr = unsafeFreezePrimArray
    {-# INLINE unsafeFreezeArr #-}
    unsafeThawArr = unsafeThawPrimArray
    {-# INLINE unsafeThawArr #-}

    copyArr marr s1 arr s2 l = copyPrimArray marr s1 arr s2 l
    {-# INLINE copyArr #-}
    copyMutableArr marr1 s1 marr2 s2 l = copyMutablePrimArray marr1 s1 marr2 s2 l
    {-# INLINE copyMutableArr #-}

    moveArr marr1 s1 marr2 s2 l = movePrimArray marr1 s1 marr2 s2 l
    {-# INLINE moveArr #-}

    cloneArr arr s l = runST (do
            marr <- newPrimArray l
            copyPrimArray marr 0 arr s l
            unsafeFreezePrimArray marr
        )
    {-# INLINE cloneArr #-}
    cloneMutableArr marr s l = do
        marr' <- newPrimArray l
        copyMutablePrimArray marr' 0 marr s l
        return marr'
    {-# INLINE cloneMutableArr #-}

    resizeMutableArr marr n = resizeMutablePrimArray marr n
    {-# INLINE resizeMutableArr #-}
    shrinkMutableArr marr n = shrinkMutablePrimArray marr n
    {-# INLINE shrinkMutableArr #-}

    sameMutableArr = sameMutablePrimArray
    {-# INLINE sameMutableArr #-}
    sizeofArr = sizeofPrimArray
    {-# INLINE sizeofArr #-}
    sizeofMutableArr = sizeofMutablePrimArray
    {-# INLINE sizeofMutableArr #-}
