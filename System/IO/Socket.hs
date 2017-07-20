
module System.IO.Socket where

data SocketStatus = SocketWaiting
                  | SocketWorking
                  | SocketClosed

data Socket = Socket
    { socketFd :: Fd
    , socketStatus :: MVar SocketStatus
    }
