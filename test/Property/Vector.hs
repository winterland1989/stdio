{-# LANGUAGE TypeApplications #-}

module Property.Vector where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck
import qualified Data.Vector as V
import Data.Word
import qualified Data.List as List

propertyVector :: TestTree
propertyVector = testGroup "vector property" [
        testProperty "unpack @Vector @Int . pack == id" . property $ \ xs ->
            (V.unpack @V.Vector @Int) (V.pack xs)  === xs
    ,   testProperty "unpack @PrimVector @Int . pack == id" . property $ \ xs ->
            (V.unpack @V.PrimVector @Int) (V.pack xs)  === xs
    ,   testProperty "unpack @PrimVector @Int . pack == id" . property $ \ xs ->
            (V.unpack @V.PrimVector @Word8) (V.pack xs)  === xs

    ,   testProperty "vector length == list length" . property $ \ xs ->
            (V.length $ V.pack @V.Vector @Int xs)  ===  List.length xs
    ,   testProperty "vector length == list length" . property $ \ xs ->
            (V.length $ V.pack @V.PrimVector @Int xs)  ===  List.length xs
    ,   testProperty "vector length == list length" . property $ \ xs ->
            (V.length $ V.pack @V.PrimVector @Word8 xs)  ===  List.length xs


    ]
