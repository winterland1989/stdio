import Foreign.C.Types

data StdStream = StdStream
    { stdStreamFD :: CInt
    , stdStreamInfo :: String
    }

instance Input File where
    inputInfo (StdStream _ info) = info
    readInput (StdStream fd path) buf len = do
#ifdef mingw32_HOST_OS
        if rtsSupportsBoundThreads
        then E.throwErrnoIfMinus1Retry callStack path $
                c_read fd buf (fromIntegral len)
        else asyncReadRawBufferPtr callStack path fd buf 0 len
      where
        asyncWriteRawBufferPtr cstack path fd buf len = do
            (l, rc) <- asyncWrite (fromIntegral fd) 0 (fromIntegral len) buf
            if l == (-1)
            then
                throwOtherErrno cstack path (Errno (fromIntegral rc))
            else return (fromIntegral l)
#else
        fromIntegral `fmap` E.throwErrnoIfMinus1RetryMayBlock callStack path    -- In theory regular file shouldn't block
            (c_read fd buf (fromIntegral len))                                -- but we use retryMayBlock anyway
            (threadWaitRead (fromIntegral fd))
#endif

