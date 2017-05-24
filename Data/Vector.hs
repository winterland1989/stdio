{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Vector where

import GHC.Exts (IsList(..))
import GHC.ST
import Data.Primitive.Types
import qualified Data.Primitive.Array as A
import qualified Data.Vector.PrimVector as P
import qualified Data.Vector.PrimArray as P
import qualified Data.Vector.BoxedVector as BV
import Data.Word

class (Arr (MArray v) (IArray v), IsList (v a)) => Vect v a where
    type MArray v :: * -> * -> *
    type IArray v :: * -> *

    create :: Int -> (forall s. MArray v s a -> ST s ()) -> v


map :: (Vect u a, Vect v b) => (a -> b) -> u a -> v b



instance Prim a => GVector (PrimVector a) a where
    type MArray (PrimVector a) = MutablePrimArray
    type IArray (PrimVector a) = PrimArray

    create = P.create



