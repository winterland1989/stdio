{-# LANGUAGE TypeApplications #-}

module Property.Vector where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck
import qualified Data.Vector as V

propertyVector :: TestTree
propertyVector = testGroup "vector property" [
        testProperty "unpack @Vector @Int . pack == id" . property $ \ xs ->
            (V.unpack @V.Vector @Int) (V.pack xs)  === xs
    ,   testProperty "unpack @PrimVector @Int . pack == id" . property $ \ xs ->
            (V.unpack @V.PrimVector @Int) (V.pack xs)  === xs

    ]
