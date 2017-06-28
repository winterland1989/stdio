{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}

module Text (text) where

import Criterion.Main
import qualified Data.ByteString as BS
import qualified "text" Data.Text as T
import qualified "stdio" Data.Text as S
import qualified "stdio" Data.Vector as V
import Control.DeepSeq
import Control.Monad
import Control.Exception (evaluate)
import Data.Monoid ((<>))
import Data.Word
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

text1000 :: T.Text
text1000 = T.replicate 1000 (T.singleton '韩')

stext1000 :: S.Text
stext1000 = S.packN 1000 (List.replicate 1000 '韩')

text :: [Benchmark]
text = List.reverse
    [ bgroup "pack" pack1000
    , bgroup "unpack" unpack1000
    , bgroup "last" last
    , bgroup "length" length
    , bgroup "map" map
    ]

unpack1000 :: [Benchmark]
unpack1000 =
    [ bench "text/unpack" $ nf T.unpack text1000
    , bench "stdio text/unpack" $ nf S.unpack stext1000
    ]

pack1000 :: [Benchmark]
pack1000 =
    [ bench "text/pack" $ nf T.pack (List.replicate 1000 '0')
    , bench "stdio text/pack" $ nf S.pack (List.replicate 1000 '0')
    ]

last :: [Benchmark]
last =
    [ bench "text/last" $ nf T.last text1000
    , bench "stdio text/last" $ nf S.last stext1000
    ]

length :: [Benchmark]
length =
    [ bench "text/length" $ nf T.length text1000
    , bench "stdio text/length" $ nf S.length stext1000
    ]

map :: [Benchmark]
map =
    [ bench "text/map" $ nf (T.map id) text1000
    , bench "stdio text/map" $ nf (S.map id) stext1000
    ]
