{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ImplicitParams #-}

module GHC.Stack.Compat
  ( -- Compatible layer of GHC.Stack
    HasCallStack
  , CallStack
  , callStack
  , prettyCallStack
  ) where

import GHC.Stack

#if !MIN_VERSION_base(4,9,0)
type HasCallStack = (?callStack :: CallStack)

callStack :: HasCallStack => CallStack
{-# INLINE callStack #-}
callStack = drop 1 $ getCallStack ?callStack

prettyCallStack :: CallStack -> String
prettyCallStack = showCallStack
#endif
