module Data.Builder.Textual where

class Write a where
    write :: a -> Builder

class Write a => FormatWrite a where
    data Format a
    format :: Format a -> a -> Builder
