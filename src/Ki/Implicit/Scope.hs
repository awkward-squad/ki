module Ki.Implicit.Scope
  ( Scope,
    Ki.Scope.cancel,
    scoped,
    Ki.Scope.wait,
    Ki.Scope.waitFor,
    Ki.Scope.waitSTM,
  )
where

import Ki.Concurrency
import Ki.Implicit.Context (Context)
import Ki.Scope (Scope)
import qualified Ki.Scope

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
