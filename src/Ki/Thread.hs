{-# LANGUAGE TypeApplications #-}

module Ki.Thread
  ( Thread (..),
    threadAsync,
    threadAsyncWithUnmask,
    threadAwait,
    threadAwaitFor,
    threadFork,
    threadFork_,
    threadForkWithUnmask,
    threadForkWithUnmask_,
  )
where

import Control.Exception (BlockedIndefinitelyOnSTM (..), Exception (fromException), SomeAsyncException, catch)
import Data.Function (on)
import Data.Maybe (isJust)
import Data.Ord (comparing)
import Ki.Context
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

threadAsync :: Scope -> IO a -> IO (Thread (Either SomeException a))
threadAsync scope action =
  threadAsyncWithRestore scope \restore -> restore action

threadAsyncWithUnmask :: Scope -> ((forall x. IO x -> IO x) -> IO a) -> IO (Thread (Either SomeException a))
threadAsyncWithUnmask scope action =
  threadAsyncWithRestore scope \restore -> restore (action unsafeUnmask)

threadAsyncWithRestore :: forall a. Scope -> ((forall x. IO x -> IO x) -> IO a) -> IO (Thread (Either SomeException a))
threadAsyncWithRestore scope action = do
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

threadAwait :: Thread a -> IO a
threadAwait thread =
  -- If *they* are deadlocked, we will *both* will be delivered a wakeup from the RTS. We want to shrug this exception
  -- off, because afterwards they'll have put to the result var. But don't shield indefinitely, once will cover this use
  -- case and prevent any accidental infinite loops.
  go `catch` \BlockedIndefinitelyOnSTM -> go
  where
    go =
      atomically (thread'Await thread)

threadAwaitFor :: Thread a -> Duration -> IO (Maybe a)
threadAwaitFor thread duration =
  timeoutSTM duration (pure . Just <$> thread'Await thread) (pure Nothing)

threadFork :: Scope -> IO a -> IO (Thread a)
threadFork scope action =
  forkWithRestore scope \restore -> restore action

threadFork_ :: Scope -> IO () -> IO ()
threadFork_ scope action =
  forkWithRestore_ scope \restore -> restore action

threadForkWithUnmask :: Scope -> ((forall x. IO x -> IO x) -> IO a) -> IO (Thread a)
threadForkWithUnmask scope action =
  forkWithRestore scope \restore -> restore (action unsafeUnmask)

threadForkWithUnmask_ :: Scope -> ((forall x. IO x -> IO x) -> IO ()) -> IO ()
threadForkWithUnmask_ scope action =
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
                readTVar (context'cancelStateVar (scope'context scope)) <&> \case
                  CancelState'NotCancelled -> True
                  CancelState'Cancelled ourToken way ->
                    case way of
                      CancelWay'Direct -> thrownToken /= ourToken
                      CancelWay'Indirect -> True
            Nothing -> pure (should exception)
