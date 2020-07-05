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
    Ki.Implicit.Fork.fork,
    Ki.Implicit.Fork.forkWithUnmask,
    {-
    -- ** Actor
    Ki.Implicit.Actor.Actor,
    Ki.Implicit.Actor.send,
    -}

    -- ** Thread
    Ki.Implicit.Thread.Thread,
    Ki.Implicit.Thread.async,
    Ki.Implicit.Thread.asyncWithUnmask,
    Ki.Implicit.Thread.await,
    Ki.Implicit.Thread.awaitSTM,
    Ki.Implicit.Thread.awaitFor,
    -- kill,

    -- * Soft-cancellation
    Ki.Implicit.Context.Context,
    Ki.Implicit.Scope.cancel,
    Ki.Implicit.Context.cancelled,
    Ki.Implicit.Context.cancelledSTM,
    Ki.Implicit.Context.unlessCancelled,

    -- * Global context
    Ki.Implicit.Context.global,

    -- * Exceptions
    pattern Ki.Implicit.Context.Cancelled,

    -- * Miscellaneous
    Seconds,
    sleep,
    timeoutSTM,
  )
where

import Ki.Concurrency
-- import qualified Ki.Implicit.Actor
import qualified Ki.Implicit.Context
import qualified Ki.Implicit.Fork
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
sleep :: Ki.Implicit.Context.Context => Seconds -> IO ()
sleep seconds =
  timeoutSTM seconds Ki.Implicit.Context.cancelledSTM (pure ())
