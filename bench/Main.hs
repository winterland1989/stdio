{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module Main (main) where

import Criterion.Main
import qualified Data.ByteString as P
import qualified Data.Bytes as B
import qualified Data.List as List

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
main = defaultMain $ List.reverse
    [ bgroup "pack/256 elems"  packSmall
    , bgroup "pack/8192 elems"  packLarge
    , bgroup "unpack" unpack
    , bgroup "map" map
    , bgroup "reverse" reverse
    , bgroup "intersperse" intersperse
    ]

packSmall :: [Benchmark]
packSmall =
    [ bench "bytestring/pack"  $ nf P.pack (List.replicate 256 128)
    , bench "bytes/pack"       $ nf B.pack (List.replicate 256 128)
    , bench "bytes/packN"      $ nf (B.packN 256) (List.replicate 256 128)
    ]

packLarge :: [Benchmark]
packLarge =
    [ bench "bytestring/pack"  $ nf P.pack (List.replicate 8192 128)
    , bench "bytes/pack"       $ nf B.pack (List.replicate 8192 128)
    , bench "bytes/packN"      $ nf (B.packN 8192) (List.replicate 8192 128)
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
