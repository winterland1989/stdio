{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Data.GArray where

import Data.Primitive.Types
import Control.Monad.Primitive

import Data.PrimArray
import Data.PrimVector as P
import GHC.ST

class  (marr :: * -> * -> *) ⟦⌁⟧ (arr :: * -> * ) | arr -> marr, marr -> arr where
    newArray :: (PrimMonad m, PrimState m ~ s) => Int -> m (marr s a)
    readArray :: (PrimMonad m, PrimState m ~ s) => Int -> m (marr s a)
    unsafeFreeze :: (PrimMonad m, PrimState m ~ s) => marr s a -> m (arr a)


instance GArray MutablePrimArray PrimArray where
    unsafeFreeze = unsafeFreeze


