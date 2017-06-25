{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Criterion.Main
import qualified Data.ByteString as B
import qualified "stdio" Data.Vector as V
import qualified Data.List as List
import qualified Data.Vector.Unboxed as VU
import Data.Word
import Control.DeepSeq
import Builder
import Bytes


main :: IO ()
main = defaultMain -- $ List.reverse
    [ bgroup "Bytes" bytes
    , bgroup "Builder" builder
    ]
