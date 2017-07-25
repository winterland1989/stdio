
module System.IO.Tcp where

import System.IO.UV
import System.IO.UVManger

data SocketStatus = SocketWaiting
                  | SocketWorking
                  | SocketClosed

data Tcp = Tcp
    { socketUVTcp :: Ptr UVTcp
    , socketLoop  :: Ptr UVLoop
    }
