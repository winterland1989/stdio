{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Foreign.PrimVector where

import qualified Data.Vector as V
import Data.Primitive.PrimArray
import Data.Primitive.ByteArray
import Data.Primitive.Types
import Foreign.Ptr
import Foreign.C
import Control.Monad.Primitive
import GHC.Types


-- | Use a 'PrimVector' as a pointer
--
-- This function will automatically create a pinned copy if original one is not pinned.
-- And it guarantee the pointer is aligned with respect to type 'a'.
--
-- The FFI function will receive an extra 'CSize' parameter which is the vector's length.
--
-- You should not mutate the content of the pointers since the original vector will be
-- changed too. If you need do that, consider using 'withMutablePrimVector'.
--
withPrimVector :: forall a b. Prim a
               => V.PrimVector a
               -> (Ptr a -> CSize -> IO b)
               -> IO b
{-# INLINABLE withPrimVector #-}
withPrimVector (V.PrimVector ba s l) f = do
    ba' <- if isPrimArrayPinned ba
        then return ba
        else do
            mba <- newAlignedPinnedPrimArray l (I# (alignment# (undefined :: a)))
            copyPrimArray mba 0 ba s l
            unsafeFreezePrimArray mba
    withPrimArrayContents ba' (\ p -> f p (fromIntegra l))

-- | Use a 'Bytes' as a pointer
--
-- The 'Bytes' are passed as it is, no encode/decode involved.
--
withBytes :: V.Bytes -> -> (CString -> IO b) -> IO b
withBytes bs f = withPrimVector bs (f . castPtr)

-- | Use a 'PrimVector' as a pointer for unsafe FFI.
--
-- This function will NOT create a pinned copy, thus you should only use it with unsafe FFI.
--
-- You should not mutate the content of the pointers since the original vector will be
-- changed too.
--
unsafeWithPrimVector :: forall a b. Prim a
                     => V.PrimVector a
                     -> (ByteArray# -> CSize -> CSize -> IO b)
                     -> IO b
{-# INLINABLE unsafeWithPrimVector #-}
unsafeWithPrimVector (V.PrimVector (PrimArray (ByteArray ba#)) s l) f =
    f ba# (fromIntegral s) (fromIntegral l)

-- | Copy a 'PrimVector' and use it as a pointer.
--
-- The mutable copy is freezed and return as a new vector.
--
withMutablePrimVector :: forall a b. Prim a
                      => V.PrimVector a
                      -> (Ptr a -> IO b)
                      -> IO (V.PrimVector a, b)
{-# INLINABLE withMutablePrimVector #-}
withMutablePrimVector (V.PrimVector ba s l) f = do
    mba <- newAlignedPinnedPrimArray l (I# (alignment# (undefined :: a)))
    copyPrimArray mba 0 ba s l
    r <- withMutablePrimArrayContents mba f
    ba' <- unsafeFreezePrimArray mba
    return (V.PrimVector ba' 0 l, r)
