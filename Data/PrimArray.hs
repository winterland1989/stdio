{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.PrimArray (
  -- * Types
  PrimArray, MutablePrimArray,

  -- * Allocation
  newPrimArray, newPinnedPrimArray, newAlignedPinnedPrimArray,

  -- * Element access
  readPrimArray, writePrimArray, indexPrimArray,

  -- * Freezing and thawing
  unsafeFreezePrimArray, unsafeThawPrimArray,

  -- * Block operations
  copyPrimArray, copyMutablePrimArray, movePrimArray,
  setPrimArray,

  -- * Information
  sizeofPrimArray, sizeofMutablePrimArray, sameMutablePrimArray,
  primArrayContents, mutablePrimArrayContents
) where


import Data.Primitive
import Control.Monad.Primitive
import Data.Typeable
import GHC.Ptr (Ptr(..))


-- | Primitive array tagged with type @a@.
--
newtype PrimArray a = PrimArray ByteArray deriving Typeable

-- | Mutable primitive array tagged with type @a@.
--
newtype MutablePrimArray s a = MutablePrimArray (MutableByteArray s) deriving Typeable

-- | Create a new mutable primitive array of the specified size.
newPrimArray :: forall m a . (PrimMonad m, Prim a) => Int -> m (MutablePrimArray (PrimState m) a)
{-# INLINE newPrimArray #-}
newPrimArray n = MutablePrimArray `fmap` (newByteArray (n*siz))
  where siz = sizeOf (undefined :: a)

-- | Create a /pinned/ byte array of the specified size and respect the primitive type's
-- alignment. The garbage collector is guaranteed not to move it.
newPinnedPrimArray :: forall m a. (PrimMonad m, Prim a) => Int -> m (MutablePrimArray (PrimState m) a)
{-# INLINE newPinnedPrimArray #-}
newPinnedPrimArray n = MutablePrimArray `fmap` (newAlignedPinnedByteArray (n*siz) align)
  where siz = sizeOf (undefined :: a)
        align = alignment (undefined :: a)

-- | Create a /pinned/ primitive array of the specified size and respect given
-- alignment. The garbage collector is guaranteed not to move it.
newAlignedPinnedPrimArray
  :: forall m a. (PrimMonad m, Prim a) => Int -> Int -> m (MutablePrimArray (PrimState m) a)
{-# INLINE newAlignedPinnedPrimArray #-}
newAlignedPinnedPrimArray n align = MutablePrimArray `fmap` (newAlignedPinnedByteArray (n*siz) align)
  where siz = sizeOf (undefined :: a)

-- | Yield a pointer to the array's data. This operation is only safe on
-- /pinned/ primitive arrays allocated by 'newAlignedPinnedPrimArray'.
primArrayContents :: PrimArray a -> Ptr a
{-# INLINE primArrayContents #-}
primArrayContents (PrimArray ba) =
    let !(Addr addr#) = (byteArrayContents ba) in Ptr addr#

-- | Yield a pointer to the array's data. This operation is only safe on
-- /pinned/ primitive arrays allocated by 'newPinnedPrimArray' or
-- 'newAlignedPinnedPrimArray'.
mutablePrimArrayContents :: MutablePrimArray s a -> Ptr a
{-# INLINE mutablePrimArrayContents #-}
mutablePrimArrayContents (MutablePrimArray mba) =
    let !(Addr addr#) = (mutableByteArrayContents mba) in Ptr addr#

-- | Check if the two arrays refer to the same memory block.
sameMutablePrimArray :: MutablePrimArray s a -> MutablePrimArray s a -> Bool
{-# INLINE sameMutablePrimArray #-}
sameMutablePrimArray (MutablePrimArray mbaA) (MutablePrimArray mbaB) = sameMutableByteArray mbaA mbaB

-- | Convert a mutable primitive array to an immutable one without copying. The
-- array should not be modified after the conversion.
unsafeFreezePrimArray
  :: (PrimMonad m) => MutablePrimArray (PrimState m) a -> m (PrimArray a)
{-# INLINE unsafeFreezePrimArray #-}
unsafeFreezePrimArray (MutablePrimArray mba) = PrimArray `fmap` (unsafeFreezeByteArray mba)

-- | Convert an immutable primitive array to a mutable one without copying. The
-- original array should not be used after the conversion.
unsafeThawPrimArray
  :: (PrimMonad m) => PrimArray a -> m (MutablePrimArray (PrimState m) a)
{-# INLINE unsafeThawPrimArray #-}
unsafeThawPrimArray (PrimArray ba) = MutablePrimArray `fmap` (unsafeThawByteArray ba)

-- | Size of the primitive array.
sizeofPrimArray :: forall a . (Prim a) => PrimArray a -> Int
{-# INLINE sizeofPrimArray #-}
sizeofPrimArray (PrimArray ba) = (sizeofByteArray ba) `quot` siz
  where siz = sizeOf (undefined :: a)

-- | Size of the mutable primitive array.
sizeofMutablePrimArray :: forall s a . (Prim a) => MutablePrimArray s a -> Int
{-# INLINE sizeofMutablePrimArray #-}
sizeofMutablePrimArray (MutablePrimArray mba) = (sizeofMutableByteArray mba) `quot` siz
  where siz = sizeOf (undefined :: a)

-- | Read a primitive value from the primitive array. The offset is given in
-- elements of type @a@.
indexPrimArray :: Prim a => PrimArray a -> Int -> a
{-# INLINE indexPrimArray #-}
indexPrimArray (PrimArray ba) = indexByteArray ba

-- | Read a primitive value from the primitive array. The offset is given in
-- elements of type @a@.
readPrimArray
  :: forall m a. (PrimMonad m, Prim a) => MutablePrimArray (PrimState m) a -> Int -> m a
{-# INLINE readPrimArray #-}
readPrimArray (MutablePrimArray mba) i = readByteArray mba i

-- | Write a primitive value to the primitive array. The offset is given in
-- elements of type @a@.
writePrimArray
  :: forall m a. (PrimMonad m, Prim a) => MutablePrimArray (PrimState m) a -> Int -> a -> m ()
{-# INLINE writePrimArray #-}
writePrimArray (MutablePrimArray mba) i x = writeByteArray mba i x

-- | Copy a slice of an immutable primitive array to a mutable primitive array.
-- The offset and length are given in elements of type @a@.
copyPrimArray :: forall m a. (PrimMonad m, Prim a)
              => MutablePrimArray (PrimState m) a -- ^ destination array
              -> Int                              -- ^ offset into destination array
              -> PrimArray a                      -- ^ source array
              -> Int                              -- ^ offset into source array
              -> Int                              -- ^ number of prims to copy
              -> m ()
copyPrimArray (MutablePrimArray dst) doff (PrimArray src) soff n =
    copyByteArray dst (doff*siz) src (soff*siz) (n*siz)
  where siz = sizeOf (undefined :: a)

-- | Copy a slice of a mutable primitive array into another array. The two slices
-- may not overlap.
-- The offset and length are given in elements of type @a@.
copyMutablePrimArray :: forall m a. (PrimMonad m, Prim a)
                     => MutablePrimArray (PrimState m) a -- ^ destination array
                     -> Int                              -- ^ offset into destination array
                     -> MutablePrimArray (PrimState m) a -- ^ source array
                     -> Int                              -- ^ offset into source array
                     -> Int                              -- ^ number of prims to copy
                     -> m ()
{-# INLINE copyMutablePrimArray #-}
copyMutablePrimArray (MutablePrimArray dst) doff (MutablePrimArray src) soff n =
    copyMutableByteArray dst (doff*siz) src (soff*siz) (n*siz)
  where siz = sizeOf (undefined :: a)

-- | Copy a slice of a mutable primitive array into another, potentially
-- overlapping array.
-- The offset and length are given in elements of type @a@.
movePrimArray :: forall m a. (PrimMonad m, Prim a)
              => MutablePrimArray (PrimState m) a -- ^ destination array
              -> Int                              -- ^ offset into destination array
              -> MutablePrimArray (PrimState m) a -- ^ source array
              -> Int                              -- ^ offset into source array
              -> Int                              -- ^ number of prims to copy
              -> m ()
{-# INLINE movePrimArray #-}
movePrimArray (MutablePrimArray dst) doff (MutablePrimArray src) soff n =
    moveByteArray dst (doff*siz) src (soff*siz) (n*siz)
  where siz = sizeOf (undefined :: a)

-- | Fill a slice of a mutable primitive array with a value.
-- The offset and length are given in elements of type @a@.
setPrimArray :: forall m a. (PrimMonad m, Prim a)
             => MutablePrimArray (PrimState m) a -- ^ array to fill
             -> Int                              -- ^ offset into array
             -> Int                              -- ^ number of values to fill
             -> a                                -- ^ value to fill with
             -> m ()
{-# INLINE setPrimArray #-}
setPrimArray (MutablePrimArray mba) s n x = setByteArray mba s n x
