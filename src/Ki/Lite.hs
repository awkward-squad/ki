module Ki.Lite
  ( -- * Scope
    scoped,
    Ki.Scope.wait,
    Ki.Scope.waitSTM,
    Ki.Scope.waitFor,
    Ki.Scope.Scope,

    -- * Spawning threads

    -- ** Fork
    Ki.Fork.fork,
    Ki.Fork.forkWithUnmask,

    -- ** Async
    Ki.Thread.async,
    Ki.Thread.asyncWithUnmask,
    Ki.Thread.await,
    Ki.Thread.awaitSTM,
    Ki.Thread.awaitFor,
    Ki.Thread.Thread,

    -- * Miscellaneous
    Ki.Timeout.timeoutSTM,
    Ki.Seconds.Seconds,

    -- * Experimental
    Ki.Experimental.Puller.puller,
    Ki.Experimental.Pusher.pusher,
  )
where

import qualified Ki.Context
import qualified Ki.Experimental.Puller
import qualified Ki.Experimental.Pusher
import qualified Ki.Fork
import Ki.Prelude
import qualified Ki.Scope
import qualified Ki.Seconds
import qualified Ki.Thread
import qualified Ki.Timeout

-- | Perform an action with a new __scope__, then /close/ the __scope__.
--
-- /Throws/:
--
--   * The first exception a __thread__ forked with 'fork' throws, if any.
--
-- ==== __Examples__
--
-- @
-- 'scoped' \\scope -> do
--   'fork' scope worker1
--   'fork' scope worker2
--   'wait' scope
-- @
scoped :: (Ki.Scope.Scope -> IO a) -> IO a
scoped =
  Ki.Scope.scoped Ki.Context.dummy
