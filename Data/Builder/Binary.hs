module Data.Builder.Binary where

class BuildB a where
    buildB :: a -> Builder
    buildLE :: a -> Builder
    buildBE :: a -> Builder
