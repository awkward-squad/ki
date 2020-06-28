module Ki.Implicit.Scope
  ( Scope,
    async,
    cancel,
    fork,
    scoped,
    wait,
  )
where

import Ki.Concurrency
import Ki.Implicit.Context (Context)
import Ki.Scope.Internal (Scope, cancel, wait)
import qualified Ki.Scope.Internal as Internal
import Ki.Thread (Thread)

async :: Scope -> (Context => (forall x. IO x -> IO x) -> IO a) -> IO (Thread a)
async scope action =
  Internal.async scope \restore -> withContext scope (action restore)

fork :: Scope -> (Context => (forall x. IO x -> IO x) -> IO ()) -> IO ()
fork scope action =
  Internal.fork scope \restore -> withContext scope (action restore)

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
scoped :: Context => (Context => Scope -> IO a) -> IO a
scoped action =
  Internal.scoped ?context \scope -> withContext scope (action scope)

withContext :: Scope -> (Context => IO a) -> IO a
withContext scope action =
  Internal.withContext scope \context -> let ?context = context in action
