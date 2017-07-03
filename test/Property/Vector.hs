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
        testProperty "unpack . pack == id" . property $ \ xs ->
            (V.unpack @V.Vector @Int) (V.pack xs)  === xs
    ,   testProperty "unpack . pack == id" . property $ \ xs ->
            (V.unpack @V.PrimVector @Int) (V.pack xs)  === xs
    ,   testProperty "unpack . pack == id" . property $ \ xs ->
            (V.unpack @V.PrimVector @Word8) (V.pack xs)  === xs

    ,   testProperty "pack == packN XX" . property $ \ xs d ->
            (V.pack @V.Vector @Int xs) === (V.packN (getPositive d) xs)
    ,   testProperty "pack == packN XX" . property $ \ xs d ->
            (V.pack @V.PrimVector @Int xs) === (V.packN (getPositive d) xs)
    ,   testProperty "pack == packN XX" . property $ \ xs d ->
            (V.pack @V.PrimVector @Word8 xs) === (V.packN (getPositive d) xs)

    ,   testProperty "reverse . pack == packR XX" . property $ \ xs ->
            (V.reverse $ V.pack @V.Vector @Int xs) === (V.packR xs)
    ,   testProperty "reverse . pack == packR XX" . property $ \ xs ->
            (V.reverse $ V.pack @V.PrimVector @Int xs) === (V.packR xs)
    ,   testProperty "reverse . pack == packR XX" . property $ \ xs ->
            (V.reverse $ V.pack @V.PrimVector @Word8 xs) === (V.packR xs)

    ,   testProperty "vector length == list length" . property $ \ xs ->
            (V.length $ V.pack @V.Vector @Int xs)  ===  List.length xs
    ,   testProperty "vector length == list length" . property $ \ xs ->
            (V.length $ V.pack @V.PrimVector @Int xs)  ===  List.length xs
    ,   testProperty "vector length == list length" . property $ \ xs ->
            (V.length $ V.pack @V.PrimVector @Word8 xs)  ===  List.length xs

    ,   testProperty "vector init == list init" . property $ \ xs ->
            (V.init . V.pack @V.Vector @Int . getNonEmpty $ xs)  ===
                (V.pack . List.init . getNonEmpty $ xs)
    ,   testProperty "vector init == list init" . property $ \ xs ->
            (V.init . V.pack @V.PrimVector @Int . getNonEmpty $ xs)  ===
                (V.pack . List.init . getNonEmpty $ xs)
    ,   testProperty "vector init == list init" . property $ \ xs ->
            (V.init . V.pack @V.PrimVector @Word8 . getNonEmpty $ xs)  ===
                (V.pack . List.init . getNonEmpty $ xs)

    ,   testProperty "vector last == list last" . property $ \ xs ->
            (V.last . V.pack @V.Vector @Int . getNonEmpty $ xs)  ===
                (List.last . getNonEmpty $ xs)
    ,   testProperty "vector last == list last" . property $ \ xs ->
            (V.last . V.pack @V.PrimVector @Int . getNonEmpty $ xs)  ===
                (List.last . getNonEmpty $ xs)
    ,   testProperty "vector last == list last" . property $ \ xs ->
            (V.last . V.pack @V.PrimVector @Word8 . getNonEmpty $ xs)  ===
                (List.last . getNonEmpty $ xs)

    ,   testProperty "vector map == list map" . property $ \ xs ->
            (V.map @V.Vector @V.Vector (+1) $ V.pack @V.Vector @Int xs) ===
                (V.pack $ List.map (+1) xs)
    ,   testProperty "vector map == list map" . property $ \ xs ->
            (V.map @V.PrimVector @V.PrimVector (+1) $ V.pack @V.PrimVector @Int xs) ===
                (V.pack $ List.map (+1) xs)
    ,   testProperty "vector map == list map" . property $ \ xs ->
            (V.map @V.PrimVector @V.PrimVector (+1) $ V.pack @V.PrimVector @Word8 xs) ===
                (V.pack $ List.map (+1) xs)
    ,   testProperty "vector map == list map" . property $ \ xs ->
            (V.map @V.PrimVector @V.Vector (+1) $ V.pack @V.PrimVector @Word8 xs) ===
                (V.pack $ List.map (+1) xs)
    ,   testProperty "vector map == list map" . property $ \ xs ->
            (V.map @V.Vector @V.PrimVector (+1) $ V.pack @V.Vector @Word8 xs) ===
                (V.pack $ List.map (+1) xs)

    ,   testProperty "vector reverse == list reverse" . property $ \ xs ->
            (V.reverse . V.pack @V.Vector @Int . getNonEmpty $ xs)  ===
                (V.pack . List.reverse . getNonEmpty $ xs)
    ,   testProperty "vector reverse == list reverse" . property $ \ xs ->
            (V.reverse . V.pack @V.PrimVector @Int . getNonEmpty $ xs)  ===
                (V.pack . List.reverse . getNonEmpty $ xs)
    ,   testProperty "vector reverse == list reverse" . property $ \ xs ->
            (V.reverse . V.pack @V.PrimVector @Word8 . getNonEmpty $ xs)  ===
                (V.pack . List.reverse . getNonEmpty $ xs)

    ]
