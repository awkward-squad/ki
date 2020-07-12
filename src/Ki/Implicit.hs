{-# LANGUAGE PatternSynonyms #-}

module Ki.Implicit
  ( -- * Scope
    Ki.Implicit.Scope.scoped,
    Ki.Implicit.Scope.wait,
    Ki.Implicit.Scope.waitSTM,
    Ki.Implicit.Scope.waitFor,
    Ki.Implicit.Scope.Scope,

    -- * Spawning threads

    -- ** Implicit
    Ki.Implicit.Fork.fork,
    Ki.Implicit.Fork.forkWithUnmask,
    {-
    -- ** Actor
    Ki.Implicit.Actor.Actor,
    Ki.Implicit.Actor.send,
    -}

    -- ** Explicit
    Ki.Implicit.Thread.async,
    Ki.Implicit.Thread.asyncWithUnmask,
    Ki.Implicit.Thread.await,
    Ki.Implicit.Thread.awaitSTM,
    Ki.Implicit.Thread.awaitFor,
    Ki.Implicit.Thread.Thread,
    -- kill,

    -- * Soft-cancellation
    Ki.Implicit.Context.Context,
    Ki.Implicit.Scope.cancel,
    Ki.Implicit.Context.cancelled,
    Ki.Implicit.Context.cancelledSTM,
    Ki.Implicit.Context.unlessCancelled,
    Ki.Implicit.Context.Cancelled (..),
    Ki.Implicit.Context.CancelToken,

    -- * Global context
    Ki.Implicit.Context.global,

    -- * Miscellaneous
    Seconds,
    sleep,
    timeoutSTM,

    -- * Experimental
    Ki.Experimental.Implicit.Actor.actor,
    Ki.Experimental.Implicit.Puller.puller,
    Ki.Experimental.Implicit.Pusher.pusher,
  )
where

import Ki.Concurrency
import qualified Ki.Experimental.Implicit.Actor
import qualified Ki.Experimental.Implicit.Puller
import qualified Ki.Experimental.Implicit.Pusher
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
