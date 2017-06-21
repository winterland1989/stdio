{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Array.Checked
  ( newArr
  , newArrWith
  , readArr
  , writeArr
  , setArr
  , indexArr
  , indexArrM
  , freezeArr
  , thawArr
  , copyArr
  , copyMutableArr
  , moveArr
  , cloneArr
  , cloneMutableArr
  , resizeMutableArr
  , shrinkMutableArr
  -- * No bound checked operations
  , A.unsafeFreezeArr
  , A.unsafeThawArr
  , A.sameMutableArr
  , A.sizeofArr
  , A.sizeofMutableArr
  -- * Boxed array type
  , Array(..)
  , MutableArray(..)
  , SmallArray(..)
  , SmallMutableArray(..)
  , A.uninitialized
  -- * Primitive array type
  , PrimArray(..)
  , MutablePrimArray(..)
  , newPinnedPrimArray, newAlignedPinnedPrimArray
  , primArrayContents, mutablePrimArrayContents
  , isPrimArrayPinned, isMutablePrimArrayPinned
  ) where

import qualified Data.Array as A
import Control.Exception (Exception(..), throw)
import Data.Typeable
import Control.Monad.Primitive
import Data.Primitive.Array
import Data.Primitive.SmallArray
import Data.Primitive.PrimArray

-- |
data OutOfBoundsException = OutOfBoundsException String deriving (Show, Typeable)
instance Exception OutOfBoundsException

check :: String -> Bool -> a -> a
check _      True  x = x
check errMsg False _ = throw (OutOfBoundsException $ "Data.Array.Checked." ++ errMsg)
{-# INLINE check #-}

newArr :: (A.Arr marr arr a, PrimMonad m, PrimState m ~ s) => Int -> m (marr s a)
newArr n = check "newArr: negative size" (n>=0) (A.newArr n)
{-# INLINE newArr #-}

newArrWith :: (A.Arr marr arr a, PrimMonad m, PrimState m ~ s) => Int -> a -> m (marr s a)
newArrWith n x = check "newArrWith: negative size" (n>=0) (A.newArrWith n x)
{-# INLINE newArrWith #-}

readArr :: (A.Arr marr arr a, PrimMonad m, PrimState m ~ s) => marr s a -> Int -> m a
readArr marr i = check "readArr: index of out bounds"
    (i>=0 && i<A.sizeofMutableArr marr)
    (A.readArr marr i)
{-# INLINE readArr #-}

writeArr :: (A.Arr marr arr a, PrimMonad m, PrimState m ~ s) => marr s a -> Int -> a -> m ()
writeArr marr i x = check "writeArr: index of out bounds"
    (i>=0 && i<A.sizeofMutableArr marr)
    (A.writeArr marr i x)
{-# INLINE writeArr #-}

setArr :: (A.Arr marr arr a, PrimMonad m, PrimState m ~ s) => marr s a -> Int -> Int -> a -> m ()
setArr marr s l x = check "setArr: index range of out bounds"
    (s>=0 && l>=0 && (s+l)<=A.sizeofMutableArr marr)
    (A.setArr marr s l x)
{-# INLINE setArr #-}

indexArr :: (A.Arr marr arr a) => arr a -> Int -> a
indexArr arr i = check "indexArr: index of out bounds"
    (i>=0 && i<A.sizeofArr arr)
    (A.indexArr arr i)
{-# INLINE indexArr #-}

indexArrM :: (A.Arr marr arr a, PrimMonad m, PrimState m ~ s) => arr a -> Int -> m a
indexArrM arr i = check "indexArrM: index of out bounds"
    (i>=0 && i<A.sizeofArr arr)
    (A.indexArrM arr i)
{-# INLINE indexArrM #-}

freezeArr :: (A.Arr marr arr a, PrimMonad m, PrimState m ~ s) => marr s a -> Int -> Int -> m (arr a)
freezeArr marr s l = check "freezeArr: index range of out bounds"
    (s>=0 && l>=0 && (s+l)<=A.sizeofMutableArr marr)
    (A.freezeArr marr s l)
{-# INLINE freezeArr #-}

thawArr :: (A.Arr marr arr a, PrimMonad m, PrimState m ~ s) => arr a -> Int -> Int -> m (marr s a)
thawArr arr s l = check "thawArr: index range of out bounds"
    (s>=0 && l>=0 && (s+l)<=A.sizeofArr arr)
    (A.thawArr arr s l)
{-# INLINE thawArr #-}

copyArr :: (A.Arr marr arr a, PrimMonad m, PrimState m ~ s) => marr s a -> Int -> arr a -> Int -> Int -> m ()
copyArr marr s1 arr s2 l  = check "copyArr: index range of out bounds"
    (s1>=0 && s2>=0 && l>=0 && (s2+l)<=A.sizeofArr arr && (s1+l)<=A.sizeofMutableArr marr)
    (A.copyArr marr s1 arr s2 l)
{-# INLINE copyArr #-}

copyMutableArr :: (A.Arr marr arr a, PrimMonad m, PrimState m ~ s) => marr s a -> Int -> marr s a -> Int -> Int -> m ()
copyMutableArr marr1 s1 marr2 s2 l = check "copyMutableArr: index range of out bounds"
    (s1>=0 && s2>=0 && l>=0 && (s2+l)<=A.sizeofMutableArr marr2 && (s1+l)<=A.sizeofMutableArr marr1)
    (A.copyMutableArr marr1 s1 marr2 s2 l)
{-# INLINE copyMutableArr #-}

moveArr :: (A.Arr marr arr a, PrimMonad m, PrimState m ~ s) => marr s a -> Int -> marr s a -> Int -> Int -> m ()
moveArr marr1 s1 marr2 s2 l = check "moveArr: index range of out bounds"
    (s1>=0 && s2>=0 && l>=0 && (s2+l)<=A.sizeofMutableArr marr2 && (s1+l)<=A.sizeofMutableArr marr1)
    (A.copyMutableArr marr1 s1 marr2 s2 l)
{-# INLINE moveArr #-}

cloneArr :: (A.Arr marr arr a) => arr a -> Int -> Int -> arr a
cloneArr arr s l = check "cloneArr: index range of out bounds"
    (s>=0 && l>=0 && (s+l)<=A.sizeofArr arr)
    (A.cloneArr arr s l)
{-# INLINE cloneArr #-}

cloneMutableArr :: (A.Arr marr arr a, PrimMonad m, PrimState m ~ s) => marr s a -> Int -> Int -> m (marr s a)
cloneMutableArr marr s l = check "cloneMutableArr: index range of out bounds"
    (s>=0 && l>=0 && (s+l)<=A.sizeofMutableArr marr)
    (A.cloneMutableArr marr s l)
{-# INLINE cloneMutableArr #-}

resizeMutableArr :: (A.Arr marr arr a, PrimMonad m, PrimState m ~ s) => marr s a -> Int -> m (marr s a)
resizeMutableArr marr n = check "resizeMutableArr: negative index"
    (n>=0)
    (A.resizeMutableArr marr n)
{-# INLINE resizeMutableArr #-}

-- | New size should be >= 0, and <= original size.
--
shrinkMutableArr :: (A.Arr marr arr a, PrimMonad m, PrimState m ~ s) => marr s a -> Int -> m ()
shrinkMutableArr marr n = check "shrinkMutableArr: size out of bounds"
    (n>=0 && n<=A.sizeofMutableArr marr)
    (A.shrinkMutableArr marr n)
{-# INLINE shrinkMutableArr #-}
