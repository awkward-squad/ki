module Ki.Implicit
  ( -- * Scope
    Ki.Implicit.Scope.scoped,
    Ki.Scope.wait,
    Ki.Scope.waitSTM,
    Ki.Scope.waitFor,
    Ki.Scope.Scope,

    -- * Spawning threads

    -- ** Fork
    Ki.Implicit.Fork.fork,
    Ki.Implicit.Fork.forkWithUnmask,

    -- ** Async
    Ki.Implicit.Thread.async,
    Ki.Implicit.Thread.asyncWithUnmask,
    Ki.Thread.await,
    Ki.Thread.awaitSTM,
    Ki.Thread.awaitFor,
    Ki.Thread.Thread,
    -- kill,

    -- * Soft-cancellation
    Ki.Implicit.Context.Context,
    Ki.Scope.cancel,
    Ki.Implicit.Context.cancelled,
    Ki.Implicit.Context.cancelledSTM,
    Ki.Implicit.Context.unlessCancelled,
    Ki.Context.Cancelled (..),
    Ki.Context.CancelToken,

    -- * Global context
    Ki.Implicit.Context.global,

    -- * Miscellaneous
    sleep,
    timeoutSTM,
    Seconds,

    -- * Experimental
    Ki.Experimental.Implicit.Puller.puller,
    Ki.Experimental.Implicit.Pusher.pusher,
  )
where

import Ki.Concurrency
import qualified Ki.Context
import qualified Ki.Experimental.Implicit.Puller
import qualified Ki.Experimental.Implicit.Pusher
import qualified Ki.Implicit.Context
import qualified Ki.Implicit.Fork
import qualified Ki.Implicit.Scope
import qualified Ki.Implicit.Thread
import Ki.Prelude
import qualified Ki.Scope
import Ki.Seconds (Seconds)
import qualified Ki.Thread
import Ki.Timeout (timeoutSTM)

-- | __Context__-aware @threadDelay@.
--
-- @
-- 'sleep' seconds =
--   'timeoutSTM seconds 'cancelledSTM' (pure ())
-- @
--
-- /Throws/:
--
--   * Throws 'Cancelled' if the current __context__ is /cancelled/.
sleep :: Ki.Implicit.Context.Context => Seconds -> IO ()
sleep seconds =
  timeoutSTM seconds Ki.Implicit.Context.cancelledSTM (pure ())
