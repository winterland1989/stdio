{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}

module Data.Array where

import Data.Primitive.Types
import Control.Monad.Primitive

import Data.Primitive.PrimArray
import Data.Primitive.Array
import GHC.ST

uninitialized :: a
uninitialized = error "Data.Array: uninitialized element accessed"

class IsList (arr a) => Arr (marr :: * -> * -> *) (arr :: * -> * ) a | arr -> marr, marr -> arr where
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

instance IsList (arr a) => Arr MutableArray Array a where
    newArr n = newArray n uninitialized
    {-# INLINE newArr #-}
    newArrWith = newArray
    {-# INLINE newArrWith #-}
    readArr = readArray
    {-# INLINE readArr #-}
    writeArr = writeArray
    {-# INLINE writeArr #-}
    setArr marr s l x = go s
      where
        !sl = s + l
        go !i | i >= sl = return ()
              | otherwise = writeArray marr i x >> go (i+1)
    {-# INLINE setArr #-}
    indexArr = indexArray
    {-# INLINE indexArr #-}
    indexArrM = indexArrayM
    {-# INLINE indexArrM #-}
    freezeArr = freezeArray
    {-# INLINE freezeArr #-}
    thawArr = thawArray
    {-# INLINE thawArr #-}
    unsafeFreezeArr = unsafeFreezeArray
    {-# INLINE unsafeFreezeArr #-}
    unsafeThawArr = unsafeThawArray
    {-# INLINE unsafeThawArr #-}

    copyArr = copyArray
    {-# INLINE copyArr #-}
    copyMutableArr = copyMutableArray
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

    cloneArr = cloneArray
    {-# INLINE cloneArr #-}
    cloneMutableArr = cloneMutableArray
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
    sizeofMutableArr = sizeofMutableArray
    {-# INLINE sizeofMutableArr #-}

instance Prim a => Arr MutablePrimArray PrimArray a where
    newArr = newPrimArray
    {-# INLINE newArr #-}
    newArrWith n x = do marr <- newPrimArray n
                        setPrimArray marr 0 n x
                        return marr
    {-# INLINE newArrWith #-}
    readArr = readPrimArray
    {-# INLINE readArr #-}
    writeArr = writePrimArray
    {-# INLINE writeArr #-}
    setArr = setPrimArray
    {-# INLINE setArr #-}
    indexArr = indexPrimArray
    {-# INLINE indexArr #-}
    indexArrM arr = return . indexPrimArray arr
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

    copyArr = copyPrimArray
    {-# INLINE copyArr #-}
    copyMutableArr = copyMutablePrimArray
    {-# INLINE copyMutableArr #-}

    moveArr = movePrimArray
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

    resizeMutableArr = resizeMutablePrimArray
    {-# INLINE resizeMutableArr #-}
    shrinkMutableArr = shrinkMutablePrimArray
    {-# INLINE shrinkMutableArr #-}

    sameMutableArr = sameMutablePrimArray
    {-# INLINE sameMutableArr #-}
    sizeofArr = sizeofPrimArray
    {-# INLINE sizeofArr #-}
    sizeofMutableArr = sizeofMutablePrimArray
    {-# INLINE sizeofMutableArr #-}
