module Data.Builder.Binary where

class Binary a where
    bin :: a -> Builder


class Binary a => BinaryEndian a where
    binHost :: a -> Builder
    binLE :: a -> Builder
    binBE :: a -> Builder


instance Prim a => Binary a where

instance Binary a => Binary [a] where
