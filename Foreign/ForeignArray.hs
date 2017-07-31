module Foreign.ForeignArray (ForeignArray) where

import Data.Array
import Foreign.ForeignPtr

-- | A 'ForeignArray' is just a 'ForeignPtr' with its length.
--
-- It uses pinned memory in the garbage collected heap, and carries no finalizers.
--
data ForeignPrimArray a = ForeignPrimArray {-# UNPACK #-} !Int {-# UNPACK #-} !(ForeignPtr a)

data MutableForeignArray s a = MutableForeignArray {-# UNPACK #-} !Int {-# UNPACK #-} !(ForeignPtr a)

newForeignArray :: Prim a => Int -> ForeignArray a

importForeignArray :: Prim a => Int -> ForeignPtr a -> ForeignArray a
