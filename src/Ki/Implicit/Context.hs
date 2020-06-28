{-# LANGUAGE PatternSynonyms #-}

module Ki.Implicit.Context
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

import Ki.Concurrency
import Ki.Context.Internal (cancel, cancelled, derive, dummy, new, pattern Cancelled)
import qualified Ki.Context.Internal as Internal

type Context =
  ?context :: Internal.Context

global :: (Context => IO a) -> IO a
global action = do
  context <- new
  let ?context = context in action
