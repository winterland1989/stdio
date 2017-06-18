{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}

module Builder where

import Criterion.Main
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Builder.Internal as B
import Control.DeepSeq
import Control.Monad
import Control.Exception (evaluate)
import Data.Monoid ((<>))

builder :: [Benchmark]
builder =
    [ bgroup "word8 100000000" word8_100000000
    , bgroup "word8 10000" word8_10000
    , bgroup "word8 32" word8_32
    ]

word8_100000000 :: [Benchmark]
word8_100000000 =
    [ bench "bytestring/toLazyByteString" $ nf BB.toLazyByteString (mconcat (replicate 100000000 (BB.word8 123)))
    , bench "bytestring/toStrict . toLazyByteString" $ nf (BL.toStrict . BB.toLazyByteString) (mconcat (replicate 100000000 (BB.word8 123)))
    , bench "stdio/buildBytesList"     $ nf B.buildBytesList (mconcat (replicate 100000000 (B.word8 123)))
    , bench "stdio/buildBytes"     $ nf B.buildBytes (mconcat (replicate 100000000 (B.word8 123)))
    , bench "stdio/buildAndRun"     $ nfIO (B.buildAndRun (void . evaluate) (mconcat (replicate 100000000 (B.word8 123))))
    ]

word8_10000 :: [Benchmark]
word8_10000 =
    [ bench "bytestring/toLazyByteString" $ nf BB.toLazyByteString (mconcat (replicate 10000 (BB.word8 123)))
    , bench "bytestring/toStrict . toLazyByteString" $ nf (BL.toStrict . BB.toLazyByteString) (mconcat (replicate 10000 (BB.word8 123)))
    , bench "stdio/buildBytesList"     $ nf B.buildBytesList (mconcat (replicate 10000 (B.word8 123)))
    , bench "stdio/buildBytes"     $ nf B.buildBytes (mconcat (replicate 10000 (B.word8 123)))
    , bench "stdio/buildAndRun"     $ nfIO (B.buildAndRun (void . evaluate) (mconcat (replicate 10000 (B.word8 123))))
    ]

word8_32 :: [Benchmark]
word8_32 =
    [ bench "bytestring/toLazyByteString" $ nf BB.toLazyByteString (mconcat (replicate 32 (BB.word8 123)))
    , bench "bytestring/toStrict . toLazyByteString" $ nf (BL.toStrict . BB.toLazyByteString) (mconcat (replicate 32 (BB.word8 123)))
    , bench "stdio/buildBytesList"     $ nf B.buildBytesList (mconcat (replicate 32 (B.word8 123)))
    , bench "stdio/buildBytes"     $ nf B.buildBytes (mconcat (replicate 32 (B.word8 123)))
    , bench "stdio/buildAndRun"     $ nfIO (B.buildAndRun (void . evaluate) (mconcat (replicate 32 (B.word8 123))))
    ]

