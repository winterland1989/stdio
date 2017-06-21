module Data.Builder.Textual where

class Textual a where
    text :: a -> Builder

class Textual a => TextualFormat a where
    data Format a
    format :: Format a -> a -> Builder
