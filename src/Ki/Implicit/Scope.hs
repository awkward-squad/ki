module Ki.Implicit.Scope
  ( Scope,
    async,
    Ki.Scope.cancel,
    fork,
    scoped,
    Ki.Scope.wait,
  )
where

import Ki.Concurrency
import Ki.Implicit.Context (Context)
import Ki.Scope (Scope)
import qualified Ki.Scope
import Ki.Thread (Thread)

async :: Scope -> (Context => (forall x. IO x -> IO x) -> IO a) -> IO (Thread a)
async scope action =
  Ki.Scope.async scope (let ?context = Ki.Scope.context scope in action)

fork :: Scope -> (Context => (forall x. IO x -> IO x) -> IO ()) -> IO ()
fork scope action =
  Ki.Scope.fork scope (let ?context = Ki.Scope.context scope in action)

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
  Ki.Scope.scoped ?context \scope -> let ?context = Ki.Scope.context scope in action scope
