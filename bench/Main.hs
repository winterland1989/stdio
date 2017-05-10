{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module Main (main) where

import Criterion.Main
import qualified Data.ByteString as P
import qualified Data.Bytes as B
import qualified Data.List as List
import qualified Data.Text.Internal.Fusion as T
import qualified Data.Text.Internal.Fusion.Common as T
import qualified Data.Vector.Unboxed as VU
import qualified Data.Array.Unboxed as AU
import Data.Word
import Control.DeepSeq

import Prelude hiding (reverse,head,tail,last,init,null
    ,length,map,lines,foldl,foldr,unlines
    ,concat,any,take,drop,splitAt,takeWhile
    ,dropWhile,span,break,elem,filter,maximum
    ,minimum,all,concatMap,foldl1,foldr1
    ,scanl,scanl1,scanr,scanr1
    ,readFile,writeFile,appendFile,replicate
    ,getContents,getLine,putStr,putStrLn,interact
    ,zip,zipWith,unzip,notElem
    )

main :: IO ()
main = defaultMain -- $ List.reverse
    [ bgroup "singleton" singleton
  -- , bgroup "pack/256 elems"  packSmall
--   , bgroup "pack/8192 elems"  packLarge
--   , bgroup "unpack" unpack
    , bgroup "map" map
    , bgroup "reverse" reverse
    , bgroup "intersperse" intersperse
    ]

singleton :: [Benchmark]
singleton =
    [ bench "bytestring/singleton" $ nf P.singleton 128
    , bench "bytes/singleton"      $ nf B.singleton 128
    ]

packSmall :: [Benchmark]
packSmall =
    [ bench "bytestring/pack"  $ nf P.pack (List.replicate 256 128)
    , bench "bytes/pack"       $ nf B.pack (List.replicate 256 128)
    , bench "vector/fromList"  $ nf VU.fromList (List.replicate 256 (128::Word8))
    , bench "bytes/packN"      $ nf (B.packN 256) (List.replicate 256 128)
    , bench "bytes/packN 64"   $ nf (B.packN 64) (List.replicate 256 128)
    ]

packLarge :: [Benchmark]
packLarge =
    [ bench "bytestring/pack"  $ nf P.pack (List.replicate 8192 128)
    , bench "bytes/pack"       $ nf B.pack (List.replicate 8192 128)
    , bench "bytes/packN"      $ nf (B.packN 8192) (List.replicate 8192 128)
    , bench "bytes/packN 64"   $ nf (B.packN 64) (List.replicate 8192 128)
    ]

unpack :: [Benchmark]
unpack =
    [ bench "bytestring/unpack"  $ nf P.unpack (P.pack $ List.replicate 1024 128)
    , bench "bytes/unpack"       $ nf B.unpack (B.pack $ List.replicate 1024 128)
    ]

map :: [Benchmark]
map =
    [ bench "bytestring/map"  $ nf (P.map (+1)) (P.pack $ List.replicate 1024 128)
    , bench "bytes/map"       $ nf (B.map (+1)) (B.pack $ List.replicate 1024 128)
    , bench "vector/map"       $ nf (VU.map (+1)) (VU.fromList $ List.replicate 1024 (128::Word8))
    , bench "bytes/map"       $ nf (B.packN 1024 . List.map (+1) . B.unpack) (B.pack $ List.replicate 1024 128)
    ]

reverse :: [Benchmark]
reverse =
    [ bench "bytestring/reverse"  $ nf P.reverse (P.pack $ List.replicate 1024 128)
    , bench "bytes/reverse"       $ nf B.reverse (B.pack $ List.replicate 1024 128)
    ]

intersperse :: [Benchmark]
intersperse =
    [ bench "bytestring/intersperse"  $ nf (P.intersperse 0) (P.pack $ List.replicate 1024 128)
    , bench "bytes/intersperse"       $ nf (B.intersperse 0) (B.pack $ List.replicate 1024 128)
    ]
