module Ki.Thread
  ( Thread,
    async,
    asyncWithUnmask,
    await,
    awaitSTM,
    awaitFor,
    kill,
  )
where

import Control.Exception (AsyncException (ThreadKilled))
import Ki.Prelude
import Ki.Scope (Scope)
import qualified Ki.Scope
import Ki.Seconds (Seconds)
import Ki.Timeout (timeoutSTM)

-- | A running __thread__.
data Thread a = Thread
  { threadId :: !ThreadId,
    action :: !(STM (Either SomeException a))
  }
  deriving stock (Functor, Generic)

instance Eq (Thread a) where
  Thread id1 _ == Thread id2 _ =
    id1 == id2

instance Ord (Thread a) where
  compare (Thread id1 _) (Thread id2 _) =
    compare id1 id2

-- | Fork a __thread__ within a __scope__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
async :: Scope -> IO a -> IO (Thread a)
async scope action =
  asyncWithRestore scope \restore -> restore action

-- | Variant of 'async' that provides the __thread__ a function that unmasks asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
asyncWithUnmask :: Scope -> ((forall x. IO x -> IO x) -> IO a) -> IO (Thread a)
asyncWithUnmask scope action =
  asyncWithRestore scope \restore -> restore (action unsafeUnmask)

asyncWithRestore :: forall a. Scope -> ((forall x. IO x -> IO x) -> IO a) -> IO (Thread a)
asyncWithRestore scope action = do
  resultVar <- newEmptyTMVarIO
  childThreadId <-
    Ki.Scope.fork scope action \result ->
      atomically (putTMVar resultVar result)
  pure (Thread childThreadId (readTMVar resultVar))

-- | Wait for a __thread__ to finish.
await :: Thread a -> IO (Either SomeException a)
await =
  atomically . awaitSTM

-- | @STM@ variant of 'await'.
awaitSTM :: Thread a -> STM (Either SomeException a)
awaitSTM Thread {action} =
  action

-- | Variant of 'await' that gives up after the given number of seconds elapses.
--
-- @
-- 'awaitFor' thread seconds =
--   'timeout' seconds (pure . Just \<$\> 'awaitSTM' thread) (pure Nothing)
-- @
awaitFor :: Thread a -> Seconds -> IO (Maybe (Either SomeException a))
awaitFor thread seconds =
  timeoutSTM seconds (pure . Just <$> awaitSTM thread) (pure Nothing)

-- | Kill a __thread__ wait for it to finish.
--
-- /Throws/:
--
--   * 'ThreadKilled' if a __thread__ attempts to kill itself.
kill :: Thread a -> IO ()
kill thread = do
  throwTo (threadId thread) ThreadKilled
  void (await thread)
