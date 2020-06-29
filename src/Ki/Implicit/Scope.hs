module Ki.Implicit.Scope
  ( Scope,
    Ki.Scope.cancel,
    fork,
    forkWithUnmask,
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
  Ki.Scope.fork scope \restore -> restore (let ?context = Ki.Scope.context scope in action)

-- | Variant of 'fork' that provides the __thread__ a function that unmasks asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
forkWithUnmask :: Scope -> (Context => (forall x. IO x -> IO x) -> IO ()) -> IO ()
forkWithUnmask scope action =
  Ki.Scope.fork scope \restore -> restore (let ?context = Ki.Scope.context scope in action unsafeUnmask)

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
