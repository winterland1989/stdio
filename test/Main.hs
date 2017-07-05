{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Test.Tasty
import           Property.Vector
import           Property.Text

main :: IO ()
main = defaultMain $ testGroup "stdio tests" [
        propertyVector
    ,   propertyText



    ]

