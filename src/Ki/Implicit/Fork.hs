module Ki.Implicit.Fork
  ( fork,
    forkWithUnmask,
  )
where

import Ki.Concurrency
import qualified Ki.Fork
import Ki.Implicit.Context (Context)
import Ki.Scope (Scope)
import qualified Ki.Scope

-- | Fork a __thread__.
--
-- If the __thread__ throws an exception, it is propagated up the call tree to the __thread__ that opened its __scope__,
-- /unless/ that exception fulfills a cancellation request.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
fork :: Scope -> (Context => IO ()) -> IO ()
fork scope action =
  Ki.Fork.fork scope (let ?context = Ki.Scope.context scope in action)

-- | Variant of 'fork' that provides the __thread__ a function that unmasks asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
forkWithUnmask :: Scope -> (Context => (forall x. IO x -> IO x) -> IO ()) -> IO ()
forkWithUnmask scope action =
  Ki.Fork.forkWithUnmask scope (let ?context = Ki.Scope.context scope in action)
