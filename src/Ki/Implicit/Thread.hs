module Ki.Implicit.Thread
  ( Thread,
    async,
    asyncWithUnmask,
    Ki.Thread.await,
    Ki.Thread.awaitSTM,
    Ki.Thread.awaitFor,
  )
where

import Ki.Concurrency
import Ki.Implicit.Context (Context)
import Ki.Scope (Scope)
import qualified Ki.Scope
import Ki.Thread (Thread)
import qualified Ki.Thread

-- | Fork a __thread__ within a __scope__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
async :: Scope -> (Context => IO a) -> IO (Thread a)
async scope action =
  Ki.Thread.async scope (let ?context = Ki.Scope.context scope in action)

-- | Variant of 'async' that provides the __thread__ a function that unmasks asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
asyncWithUnmask :: Scope -> (Context => (forall x. IO x -> IO x) -> IO a) -> IO (Thread a)
asyncWithUnmask scope action =
  Ki.Thread.asyncWithUnmask scope (let ?context = Ki.Scope.context scope in action)
