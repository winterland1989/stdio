module Main where

import GHC.IO.FD
import Foreign.Marshal.Alloc
import Control.Monad
import Control.Concurrent.Async (forConcurrently_)
import System.Environment
import Data.Bits
import GHC.IO.Device
import GHC.IO.IOMode
import Prelude hiding (read)

main :: IO ()
main = do
    [file] <- getArgs
    forConcurrently_ [0..100] $ \ i -> do
        let file' = file ++ "-" ++ show i
        (fd, _) <- openFile file ReadMode True
        (fd', _) <- openFile file' ReadWriteMode True
        loop fd fd'
        close fd
        close fd'

  where
    loop fd fd' = do
        ptr <- mallocBytes 1024
        siz <- read fd ptr 1024
        write fd' ptr 1024
        free ptr
        case siz `compare` 0 of
            LT -> error ("error:" ++ show siz)
            EQ -> return ()
            GT -> loop fd fd'
