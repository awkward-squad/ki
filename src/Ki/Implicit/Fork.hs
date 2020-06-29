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

-- | Variant of 'async' that does not return a handle to the __thread__.
--
-- If the __thread__ throws an unexpected exception, the exception is propagated up the call tree to the __thread__ that
-- opened its __scope__.
--
-- There is one expected exception the __thread__ may throw that will not be propagated up the call tree:
--
--   * 'Cancelled', as when the __thread__ voluntarily capitulates after observing a /cancellation/ request.
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
