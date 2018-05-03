{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}

{-|
Module      : System.IO.TTY
Description : TCP or IPC servers and clients
Copyright   : (c) Winterland, 2018
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

This module provides an API for opening tty as 'UVStream'. In most case, it will not be necessary to use this module directly

-}

module System.IO.TTY(
    UVStream
  , initTTY
  ) where

import System.IO.UV.Internal



initTTY :: Int ->
foreign import ccall unsafe uv_tty_init :: Ptr UVLoop -> Ptr UVHandle -> CInt -> CInt -> IO CInt

stdin :: UVStream
stdin = unsafePerformIO $

stdout :: UVStream

stderr :: UVStream

