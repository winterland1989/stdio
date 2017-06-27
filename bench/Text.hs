{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}

module Text (text) where

import Criterion.Main
import qualified Data.ByteString as BS
import qualified "text" Data.Text as T
import qualified "stdio" Data.Text as ST
import qualified "stdio" Data.Vector as V
import Control.DeepSeq
import Control.Monad
import Control.Exception (evaluate)
import Data.Monoid ((<>))
import Data.Word
import qualified Data.List as List

text1000 :: T.Text
text1000 = T.replicate 1000 (T.singleton '韩')

stext1000 :: ST.Text
stext1000 = ST.packN 1000 (List.replicate 1000 '韩')

text :: [Benchmark]
text =
    [ bgroup "pack" pack1000
    , bgroup "unpack" unpack1000
    ]

unpack1000 :: [Benchmark]
unpack1000 =
    [ bench "text/unpack" $ nf T.unpack text1000
    , bench "stdio text/unpack" $ nf ST.unpack stext1000
    ]

pack1000 :: [Benchmark]
pack1000 =
    [ bench "text/pack" $ nf T.pack (List.replicate 1000 '0')
    , bench "stdio text/pack" $ nf ST.pack (List.replicate 1000 '0')
    ]

