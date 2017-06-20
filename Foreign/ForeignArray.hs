module Foreign.ForeignArray where

data ForeignArray a = ForeignArray Addr# Addr# ForeignPtrContents
