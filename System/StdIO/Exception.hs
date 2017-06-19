{-# LANGUAGE DeriveDataTypeable #-}

module System.StdIO.Exception where

import Control.Exception
import Data.Typeable
import Foreign.C.Error

data IOException = forall e . Exception e => IOException e
    deriving Typeable

instance Show IOException where
    show (IOException e) = show e

instance Exception IOException

ioExceptionToException :: Exception e => e -> SomeException
ioExceptionToException = toException . IOException

ioExceptionFromException :: Exception e => SomeException -> Maybe e
ioExceptionFromException x = do
    IOException a <- fromException x
    cast a


data StdIOException = StdIOException Errno
