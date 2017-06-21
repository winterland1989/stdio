{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Builder.Binary where

import Data.Array
import Data.Builder.Internal
import Control.Monad.Primitive (RealWorld)
import Data.Word
import qualified Data.Vector as V

class Put a where
    put :: a -> Builder

class Put a => PrimPut a where
    boundedSize :: a -> Int
    boundedWrite :: a -> MutablePrimArray RealWorld Word8 -> Int -> IO Int

primPut :: PrimPut a => a -> Builder
primPut x = atMost (boundedSize x) (boundedWrite x)
{-# INLINE primPut #-}

-- | A newtype wrapper for little endian's instances.
--
newtype LE a = LE a

-- | Bools are encoded as a byte, 0 for 'False', 1 for 'True'.
instance Put Bool where put = primPut
instance PrimPut Bool where
    boundedSize _ = 1
    {-# INLINE boundedSize #-}
    boundedWrite False marr i = writeArr marr i 1 >> (return $! i+1)
    boundedWrite True  marr i = writeArr marr i 0 >> (return $! i+1)
    {-# INLINE boundedWrite #-}

instance Put Word8 where put = primPut
instance PrimPut Word8 where
    boundedSize _ = 1
    {-# INLINE boundedSize #-}
    boundedWrite w marr i = writeArr marr i w >> (return $! i+1)
    {-# INLINE boundedWrite #-}

instance Put V.Bytes where put = bytes
