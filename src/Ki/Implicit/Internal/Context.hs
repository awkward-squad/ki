{-# LANGUAGE PatternSynonyms #-}

module Ki.Implicit.Internal.Context
  ( -- * Context
    Context,
    dummy,
    global,
    derive,
    cancel,
    cancelled,
    pattern Cancelled,
  )
where

import Ki.Internal.Concurrency
import Ki.Internal.Context.Internal (cancel, cancelled, derive, dummy, new, pattern Cancelled)
import qualified Ki.Internal.Context.Internal as Internal

type Context =
  ?context :: Internal.Context

global :: (Context => IO a) -> IO a
global action = do
  context <- new
  let ?context = context in action
