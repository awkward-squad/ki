{-# LANGUAGE TypeApplications #-}

module Ki.Thread
  ( Thread (..),
    async,
    asyncWithUnmask,
    await,
    awaitSTM,
    awaitFor,
    fork,
    fork_,
    forkWithUnmask,
    forkWithUnmask_,
  )
where

import Control.Exception (Exception (fromException), SomeAsyncException)
import Data.Function (on)
import Data.Maybe (isJust)
import Data.Ord (comparing)
import Ki.Context
import Ki.Ctx
import Ki.Duration (Duration)
import Ki.Prelude
import Ki.Scope (Scope (..), ScopeClosing (..), ThreadFailed (..), scopeFork)
import Ki.Timeout (timeoutSTM)

-- | A running __thread__.
data Thread a = Thread
  { thread'Await :: !(STM a),
    thread'Id :: {-# UNPACK #-} !ThreadId
  }
  deriving stock (Functor)

instance Eq (Thread a) where
  (==) =
    (==) `on` thread'Id

instance Ord (Thread a) where
  compare =
    comparing thread'Id

-- | Create a __thread__ within a __scope__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
async :: Scope -> IO a -> IO (Thread (Either SomeException a))
async scope action =
  asyncWithRestore scope \restore -> restore action

-- | Variant of 'async' that provides the __thread__ a function that unmasks asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
asyncWithUnmask :: Scope -> ((forall x. IO x -> IO x) -> IO a) -> IO (Thread (Either SomeException a))
asyncWithUnmask scope action =
  asyncWithRestore scope \restore -> restore (action unsafeUnmask)

asyncWithRestore :: forall a. Scope -> ((forall x. IO x -> IO x) -> IO a) -> IO (Thread (Either SomeException a))
asyncWithRestore scope action = do
  parentThreadId <- myThreadId
  resultVar <- newEmptyTMVarIO
  thread'Id <-
    scopeFork scope action \result -> do
      case result of
        Left exception -> maybePropagateException scope parentThreadId exception isAsyncException
        Right _ -> pure ()
      putTMVarIO resultVar result -- even put async exceptions that we propagated
  pure
    Thread
      { thread'Await = readTMVar resultVar,
        thread'Id
      }
  where
    isAsyncException :: SomeException -> Bool
    isAsyncException =
      isJust . fromException @SomeAsyncException

-- | Wait for a __thread__ to finish.
await :: Thread a -> IO a
await =
  atomically . awaitSTM

-- | @STM@ variant of 'await'.
awaitSTM :: Thread a -> STM a
awaitSTM =
  thread'Await

-- | Variant of 'await' that gives up after the given duration.
awaitFor :: Thread a -> Duration -> IO (Maybe a)
awaitFor thread duration =
  timeoutSTM duration (pure . Just <$> awaitSTM thread) (pure Nothing)

-- | Create a __thread__ within a __scope__.
--
-- If the __thread__ throws an exception, the exception is immediately propagated up the call tree to the __thread__
-- that opened its __scope__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
fork :: Scope -> IO a -> IO (Thread a)
fork scope action =
  forkWithRestore scope \restore -> restore action

-- | Variant of 'fork' that does not return a handle to the created __thread__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
fork_ :: Scope -> IO () -> IO ()
fork_ scope action =
  forkWithRestore_ scope \restore -> restore action

-- | Variant of 'fork' that provides the __thread__ a function that unmasks asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
forkWithUnmask :: Scope -> ((forall x. IO x -> IO x) -> IO a) -> IO (Thread a)
forkWithUnmask scope action =
  forkWithRestore scope \restore -> restore (action unsafeUnmask)

-- | Variant of 'forkWithUnmask' that does not return a handle to the created __thread__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
forkWithUnmask_ :: Scope -> ((forall x. IO x -> IO x) -> IO ()) -> IO ()
forkWithUnmask_ scope action =
  forkWithRestore_ scope \restore -> restore (action unsafeUnmask)

forkWithRestore :: Scope -> ((forall x. IO x -> IO x) -> IO a) -> IO (Thread a)
forkWithRestore scope action = do
  parentThreadId <- myThreadId
  resultVar <- newEmptyTMVarIO
  childThreadId <-
    scopeFork scope action \case
      Left exception ->
        -- Intentionally don't fill the result var.
        --
        -- Prior to 0.2.0, we did put a 'Left exception' in the result var, so that if another thread awaited it, we'd
        -- promptly deliver them the exception that brought this thread down. However, that exception was *wrapped* in
        -- a 'ThreadFailed' exception, so the caller could distinguish between async exceptions *delivered to them* and
        -- async exceptions coming *synchronously* out of the call to 'await'.
        --
        -- At some point I reasoned that if one is following some basic structured concurrency guidelines, and not doing
        -- weird/complicated things like passing threads around, then it is likely that a failed forked thread is just
        -- about to propagate its exception to all callers of 'await' (presumably, its direct parent).
        --
        -- Might GHC deliver a BlockedIndefinitelyOnSTM in the meantime, though?
        maybePropagateException scope parentThreadId exception (const True)
      Right result -> putTMVarIO resultVar result
  pure
    Thread
      { thread'Await = readTMVar resultVar,
        thread'Id = childThreadId
      }

forkWithRestore_ :: Scope -> ((forall x. IO x -> IO x) -> IO ()) -> IO ()
forkWithRestore_ scope action = do
  parentThreadId <- myThreadId
  _childThreadId <-
    scopeFork scope action \case
      Left exception -> maybePropagateException scope parentThreadId exception (const True)
      Right () -> pure ()
  pure ()

maybePropagateException :: Scope -> ThreadId -> SomeException -> (SomeException -> Bool) -> IO ()
maybePropagateException scope parentThreadId exception should =
  whenM shouldPropagateException (throwTo parentThreadId (ThreadFailed exception))
  where
    shouldPropagateException :: IO Bool
    shouldPropagateException =
      case fromException exception of
        -- Our scope is (presumably) closing, so don't propagate this exception that (presumably) just came from our
        -- parent. But if our scope's not closed, that means this 'ScopeClosing' definitely came from somewhere else...
        Just ScopeClosing -> (/= -1) <$> readTVarIO (scope'startingVar scope)
        Nothing ->
          case fromException exception of
            -- We (presumably) are honoring our own cancellation request, so don't propagate that either.
            -- It's a bit complicated looking because we *do* want to throw this token if we (somehow) threw it
            -- "inappropriately" in the sense that it wasn't ours to throw - it was smuggled from elsewhere.
            Just thrownToken ->
              atomically do
                context'cancelState (scope'context scope) <&> \case
                  CancelState'NotCancelled -> True
                  CancelState'Cancelled ourToken way ->
                    case way of
                      CancelWay'Direct -> thrownToken /= ourToken
                      CancelWay'Indirect -> True
            Nothing -> pure (should exception)
