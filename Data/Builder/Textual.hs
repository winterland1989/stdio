module Data.Builder.Textual where


class BuildT a where
    buildT :: a -> Builder


class BuildT a => BuildF a where
    data Format a
    build :: Format a -> a -> Builder
