module Ki.Lite
  ( -- * Scope
    Scope,
    scoped,
    Ki.Implicit.wait,
    Ki.Implicit.waitSTM,
    Ki.Implicit.waitFor,

    -- * Thread
    Thread,
    async,
    asyncWithUnmask,
    fork,
    forkWithUnmask,
    Ki.Implicit.await,
    Ki.Implicit.awaitSTM,
    Ki.Implicit.awaitFor,
    -- kill,

    -- * Miscellaneous
    Seconds,
    Ki.Implicit.timeoutSTM,
  )
where

import Ki.Concurrency
import qualified Ki.Context
import Ki.Implicit (Scope, Seconds, Thread)
import qualified Ki.Implicit

-- | Fork a __thread__ within a __scope__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
async :: Scope -> IO a -> IO (Thread a)
async =
  Ki.Implicit.async

-- | Variant of 'async' that provides the __thread__ a function that unmasks asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
asyncWithUnmask :: Scope -> ((forall x. IO x -> IO x) -> IO a) -> IO (Thread a)
asyncWithUnmask =
  Ki.Implicit.asyncWithUnmask

-- | Variant of 'async' that does not return a handle to the __thread__.
--
-- If the __thread__ throws an exception, the exception is propagated up the call tree to the __thread__ that opened its
-- __scope__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
fork :: Scope -> IO () -> IO ()
fork =
  Ki.Implicit.fork

-- | Variant of 'fork' that provides the __thread__ a function that unmasks asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
forkWithUnmask :: Scope -> ((forall x. IO x -> IO x) -> IO ()) -> IO ()
forkWithUnmask =
  Ki.Implicit.forkWithUnmask

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
scoped :: (Scope -> IO a) -> IO a
scoped action =
  let ?context = Ki.Context.dummy in Ki.Implicit.scoped action
