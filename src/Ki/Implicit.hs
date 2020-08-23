module Ki.Implicit
  ( -- * Scope
    Ki.Implicit.Scope.scoped,
    Ki.Scope.wait,
    Ki.Scope.waitSTM,
    Ki.Scope.waitFor,
    Ki.Scope.Scope,

    -- * Spawning threads

    -- ** Fork
    Ki.Implicit.Thread.fork,
    Ki.Implicit.Thread.forkWithUnmask,

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
    Ki.Duration.Duration,
    Ki.Duration.microseconds,
    Ki.Duration.milliseconds,
    Ki.Duration.seconds,

    -- * Experimental
    Ki.Experimental.Implicit.Puller.puller,
    Ki.Experimental.Implicit.Pusher.pusher,
  )
where

import Ki.Concurrency
import qualified Ki.Context
import qualified Ki.Duration
import qualified Ki.Experimental.Implicit.Puller
import qualified Ki.Experimental.Implicit.Pusher
import qualified Ki.Implicit.Context
import qualified Ki.Implicit.Scope
import qualified Ki.Implicit.Thread
import Ki.Prelude
import qualified Ki.Scope
import qualified Ki.Thread
import Ki.Timeout (timeoutSTM)

-- | __Context__-aware @threadDelay@.
--
-- @
-- 'sleep' duration =
--   'timeoutSTM duration 'cancelledSTM' (pure ())
-- @
--
-- /Throws/:
--
--   * Throws 'Cancelled' if the current __context__ is /cancelled/.
sleep :: Ki.Implicit.Context.Context => Ki.Duration.Duration -> IO ()
sleep duration =
  timeoutSTM duration Ki.Implicit.Context.cancelledSTM (pure ())
