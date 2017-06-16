{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}

module Builder where

import Criterion.Main
import qualified Data.ByteString.Builder as BB
import qualified Data.Builder.Internal as B
import Control.DeepSeq
import Control.Monad
import Control.Exception (evaluate)

builder :: [Benchmark]
builder =
    [ bgroup "1000000 word8" word8
    ]

word8 :: [Benchmark]
word8 =
    [ bench "bytestring/word8" $ nf BB.toLazyByteString (mconcat (replicate 1000000 (BB.word8 123)))
    , bench "stdio/word8"     $ nf B.buildBytesList (mconcat (replicate 1000000 (B.word8 123)))
    , bench "stdio/word8"     $ nf B.buildBytes (mconcat (replicate 1000000 (B.word8 123)))
    , bench "stdio/word8"     $ nfIO (B.buildAndRun (void . evaluate) (mconcat (replicate 1000000 (B.word8 123))))
    ]
