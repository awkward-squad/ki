{-# LANGUAGE PatternSynonyms #-}

module Ki.Implicit
  ( -- * Scope
    Ki.Implicit.Scope.Scope,
    Ki.Implicit.Scope.scoped,
    Ki.Implicit.Scope.wait,
    Ki.Implicit.Scope.waitSTM,
    Ki.Implicit.Scope.waitFor,

    -- * Spawning threads

    -- ** Fork
    Ki.Implicit.Scope.fork,
    Ki.Implicit.Scope.forkWithUnmask,

    -- ** Actor
    Ki.Implicit.Actor.Actor,
    Ki.Implicit.Actor.send,

    -- ** Thread
    Ki.Implicit.Thread.Thread,
    Ki.Implicit.Thread.async,
    Ki.Implicit.Thread.asyncWithUnmask,
    Ki.Implicit.Thread.await,
    Ki.Implicit.Thread.awaitSTM,
    Ki.Implicit.Thread.awaitFor,
    -- kill,

    -- * Soft-cancellation
    Context,
    Ki.Implicit.Scope.cancel,
    cancelled,
    cancelledSTM,
    unlessCancelled,

    -- * Global context
    global,

    -- * Exceptions
    pattern Cancelled,

    -- * Miscellaneous
    Seconds,
    sleep,
    timeoutSTM,
  )
where

import Ki.Concurrency
import qualified Ki.Implicit.Actor
import Ki.Implicit.Context (Context, cancelled, cancelledSTM, global, unlessCancelled, pattern Cancelled)
import qualified Ki.Implicit.Scope
import qualified Ki.Implicit.Thread
import Ki.Prelude
import Ki.Seconds (Seconds)
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
sleep :: Context => Seconds -> IO ()
sleep seconds =
  timeoutSTM seconds cancelledSTM (pure ())
