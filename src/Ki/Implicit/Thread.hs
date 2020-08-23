module Ki.Implicit.Thread
  ( async,
    asyncWithUnmask,
    fork,
    forkWithUnmask,
  )
where

import qualified Ki.Implicit.Context
import Ki.Prelude
import qualified Ki.Scope
import qualified Ki.Thread

-- | Fork a __thread__ within a __scope__ to compute a value concurrently.
--
-- If the __thread__ throws an exception, it is /not/ propagated up the call tree, but rather can be observed by
-- 'await'.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
async :: Ki.Scope.Scope -> (Ki.Implicit.Context.Context => IO a) -> IO (Ki.Thread.Thread (Either SomeException a))
async scope action =
  Ki.Thread.async scope (let ?context = Ki.Scope.context scope in action)

-- | Variant of 'async' that provides the __thread__ a function that unmasks asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
asyncWithUnmask ::
  Ki.Scope.Scope ->
  (Ki.Implicit.Context.Context => (forall x. IO x -> IO x) -> IO a) ->
  IO (Ki.Thread.Thread (Either SomeException a))
asyncWithUnmask scope action =
  Ki.Thread.asyncWithUnmask scope (let ?context = Ki.Scope.context scope in action)

-- | Fork a __thread__.
--
-- If the __thread__ throws an exception, it is propagated up the call tree to the __thread__ that opened its __scope__,
-- /unless/ that exception fulfills a cancellation request.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
fork :: Ki.Scope.Scope -> (Ki.Implicit.Context.Context => IO a) -> IO (Ki.Thread.Thread a)
fork scope action =
  Ki.Thread.fork scope (let ?context = Ki.Scope.context scope in action)

-- | Variant of 'fork' that provides the __thread__ a function that unmasks asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
forkWithUnmask ::
  Ki.Scope.Scope ->
  (Ki.Implicit.Context.Context => (forall x. IO x -> IO x) -> IO a) ->
  IO (Ki.Thread.Thread a)
forkWithUnmask scope action =
  Ki.Thread.forkWithUnmask scope (let ?context = Ki.Scope.context scope in action)
