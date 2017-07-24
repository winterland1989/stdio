module System.IO.TTY where

import System.IO.UVManager
import System.IO.UV

data StdStream = StdStream
    { stdStreamFD :: CInt
    , stdStreamInfo :: String
    }

