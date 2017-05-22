{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module Main (main) where

import Criterion.Main
import qualified Data.ByteString as B
import qualified Data.PrimVector as P
import qualified Data.List as List
import qualified Data.Vector.Unboxed as V
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

list100, list1000, list10000 :: [Word8]
list100 = List.replicate 100 127
list1000 = List.replicate 1000 127
list10000 = List.replicate 10000 127


vector100, vector1000, vector10000 :: V.Vector Word8
vector100 = V.fromList list100
vector1000 = V.fromList list1000
vector10000 = V.fromList list10000

bytes100, bytes1000, bytes10000 :: P.Bytes
bytes100 = P.pack list100
bytes1000 = P.pack list1000
bytes10000 = P.pack list10000

bytestring100, bytestring1000, bytestring10000 :: B.ByteString
bytestring100 = B.pack list100
bytestring1000 = B.pack list1000
bytestring10000 = B.pack list10000

wordZ :: Word8
wordZ = 0

main :: IO ()
main = defaultMain $ List.reverse
    [ bgroup "singleton" singleton
    , bgroup "pack/100 elems"  packSmall
    , bgroup "pack/10000 elems"  packLarge
    , bgroup "unpack" unpack
    , bgroup "map" map
    , bgroup "reverse" reverse
    , bgroup "intersperse" intersperse
    , bgroup "intercalate" intercalate
    , bgroup "foldl" foldl
    , bgroup "foldl'" foldl'
    , bgroup "foldr" foldr
    , bgroup "foldr'" foldr'
    , bgroup "concatMap" concatMap
    , bgroup "all" all
    , bgroup "all" any
    , bgroup "scanl1" scanl1
    , bgroup "scanr1" scanr1
    ]

singleton :: [Benchmark]
singleton =
    [ bench "bytestring/singleton" $ nf B.singleton 128
    , bench "bytes/singleton"      $ nf P.singleton (128 :: Word8)
    ]

packSmall :: [Benchmark]
packSmall =
    [ bench "bytestring/pack"  $ nf B.pack list100
    , bench "vector/fromList"  $ nf V.fromList list100
    , bench "bytes/pack"       $ nf P.pack list100
    , bench "bytes/packN 64"   $ nf (P.packN 64)  list100
    , bench "bytes/packN 100"      $ nf (P.packN 100) list100
    , bench "bytes/packR"      $ nf P.packR list100
    ]

packLarge :: [Benchmark]
packLarge =
    [ bench "bytestring/pack"   $ nf B.pack list10000
    , bench "vector/fromList"   $ nf V.fromList list10000
    , bench "bytes/pack"        $ nf P.pack list10000
    , bench "bytes/packN 64"    $ nf (P.packN 64) list10000
    , bench "bytes/packN 10000" $ nf (P.packN 10000) list10000
    , bench "bytes/packR"       $ nf P.packR list10000
    , bench "bytes/packN 10000/fused" $ nf (\ n -> P.packN n (List.replicate n (128 :: Word8))) 10000
    ]

unpack :: [Benchmark]
unpack =
    [ bench "bytestring/unpack"  $ nf B.unpack bytestring1000
    , bench "bytes/unpack"       $ nf P.unpack bytes1000
    ]

map :: [Benchmark]
map =
    [ bench "bytestring/map"  $ nf (\ bs -> B.map (+1) bs) bytestring1000
    , bench "vector/map"      $ nf (V.map (+1)) vector1000
    , bench "bytes/map"       $ nf (P.map (+1)) bytes1000
    , bench "bytes/pack . List.map f . unpack" $
        nf (P.packN 1000 . List.map (+1) . P.unpack) bytes1000
    ]

reverse :: [Benchmark]
reverse =
    [ bench "bytestring/reverse"  $ nf B.reverse bytestring1000
    , bench "vector/reverse"      $ nf V.reverse vector1000
    , bench "bytes/reverse"       $ nf P.reverse bytes1000
    ]

intersperse :: [Benchmark]
intersperse =
    [ bench "bytestring/intersperse"  $ nf (B.intersperse 0) bytestring1000
    , bench "bytes/intersperse"       $ nf (P.intersperse 0) bytes1000
    ]

intercalate :: [Benchmark]
intercalate =
    [ bench "bytestring/intercalate"  $
        nf (B.intercalate bytestring100) (List.replicate 10 bytestring1000)
    , bench "bytes/intercalate"       $
        nf (P.intercalate bytes100) (List.replicate 10 bytes1000)
    , bench "bytestring/intercalate/rule" $
        nf (B.intercalate (B.singleton 0)) [bytestring1000, bytestring1000]
    , bench "bytestring/intercalateElem"  $
        nf (P.intercalateElem 0) [bytes1000, bytes1000]
    ]

foldl' :: [Benchmark]
foldl' =
    [ bench "bytestring/foldl'" $ nf (\ x -> B.foldl' (+) wordZ x) bytestring1000
    , bench "vector/foldl'"     $ nf (V.foldl' (+) wordZ) vector1000
    , bench "bytes/foldl'"      $ nf (P.foldl' (+) wordZ) bytes1000
    ]

foldl :: [Benchmark]
foldl =
    [ bench "bytestring/foldl" $ nf (\ x -> B.foldl (+) wordZ x) bytestring1000
    , bench "vector/foldl"     $ nf (V.foldl (+) wordZ) vector1000
    , bench "bytes/foldl"      $ nf (P.foldl (+) wordZ) bytes1000
    ]

foldr' :: [Benchmark]
foldr' =
    [ bench "bytestring/foldr'" $ nf (\ x -> B.foldr' (+) wordZ x) bytestring1000
    , bench "vector/foldr'"     $ nf (V.foldr' (+) wordZ) vector1000
    , bench "bytes/foldr'"      $ nf (P.foldr' (+) wordZ) bytes1000
    ]

foldr :: [Benchmark]
foldr =
    [ bench "bytestring/foldr" $ nf (\ x -> B.foldr (+) wordZ x) bytestring1000
    , bench "vector/foldr"     $ nf (V.foldr (+) wordZ) vector1000
    , bench "bytes/foldr"      $ nf (P.foldr (+) wordZ) bytes1000
    ]

concatMap :: [Benchmark]
concatMap =
    [ bench "bytestring/concatMap" $ nf (B.concatMap (const bytestring100)) bytestring1000
    , bench "vector/concatMap"     $ nf (V.concatMap (const vector100)) vector1000
    , bench "bytes/concatMap"      $ nf (P.concatMap (const bytes100)) bytes1000
    ]

all :: [Benchmark]
all =
    [ bench "bytestring/all" $ nf (B.all odd) bytestring1000
    , bench "vector/all"     $ nf (V.all odd) vector1000
    , bench "bytes/all"      $ nf (P.all odd) bytes1000
    ]

any :: [Benchmark]
any =
    [ bench "bytestring/any" $ nf (B.any even) bytestring1000
    , bench "vector/any"     $ nf (V.any even) vector1000
    , bench "bytes/any"      $ nf (P.any even) bytes1000
    ]

scanl1 :: [Benchmark]
scanl1 =
    [ bench "bytestring/scanl1" $ nf (\x-> B.scanl1 (+) x) bytestring1000
    , bench "vector/scanl1"     $ nf (V.scanl1 (+)) vector1000
    , bench "bytes/scanl1"      $ nf (P.scanl1 (+)) bytes1000
    ]

scanr1 :: [Benchmark]
scanr1 =
    [ bench "bytestring/scanr1" $ nf (\x -> B.scanr1 (+) x) bytestring1000
    , bench "vector/scanr1"     $ nf (V.scanr1 (+)) vector1000
    , bench "bytes/scanr1"      $ nf (P.scanr1 (+)) bytes1000
    ]
