module Ki.Internal.Context
  ( -- * Context
    Context,
    dummy,
    global,
    derive,
    cancel,
    cancelled,
    CancelToken (..),
    Cancelled (..),
  )
where

import Ki.Internal.Concurrency
import Ki.Internal.Context.Internal (CancelToken (..), Cancelled (..), cancel, cancelled, derive, dummy, new)
import qualified Ki.Internal.Context.Internal as Internal

type Context =
  ?context :: Internal.Context

global :: (Context => IO a) -> IO a
global action = do
  context <- new
  let ?context = context in action
