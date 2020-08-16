module Ki.Implicit.Scope
  ( scoped,
  )
where

import Ki.Concurrency
import Ki.Implicit.Context (Context)
import Ki.Scope (Scope)
import qualified Ki.Scope

-- | Open a __scope__, perform an @IO@ action with it, then close it.
--
-- When the __scope__ is closed, all remaining __threads__ forked within it are killed.
--
-- It is generally not advised to pass a __scope__ into a function, or share it amongst __threads_, as this takes the
-- "structure" out of "structured concurrency".
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
