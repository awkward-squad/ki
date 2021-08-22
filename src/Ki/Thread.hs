{-# LANGUAGE TypeApplications #-}

module Ki.Internal.Thread
  ( Thread (..),
    threadAsync,
    threadAsyncIO,
    threadAsyncWithUnmask,
    threadAsyncWithUnmaskIO,
    threadAwait,
    threadAwaitIO,
    threadAwaitFor,
    threadAwaitForIO,
    threadFork,
    threadForkIO,
    threadFork_,
    threadForkIO_,
    threadForkWithUnmask,
    threadForkWithUnmaskIO,
    threadForkWithUnmask_,
    threadForkWithUnmaskIO_,
  )
where

import Control.Exception (BlockedIndefinitelyOnSTM (..), Exception (fromException), SomeAsyncException, catch)
import Control.Monad.IO.Unlift (MonadUnliftIO (withRunInIO))
import Data.Function (on)
import Data.Maybe (isJust)
import Data.Ord (comparing)
import Ki.Internal.Duration (Duration)
import Ki.Internal.Prelude
import Ki.Internal.Scope
  ( Scope (..),
    ScopeClosing (..),
    ThreadFailed (..),
    scopeFork,
  )
import Ki.Internal.Timeout (timeoutSTM)

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

threadAsync :: MonadUnliftIO m => Scope -> m a -> m (Thread (Either SomeException a))
threadAsync =
  unliftedFork threadAsyncIO
{-# INLINE threadAsync #-}

threadAsyncIO :: Scope -> IO a -> IO (Thread (Either SomeException a))
threadAsyncIO scope action =
  threadAsyncWithRestoreIO scope \restore -> restore action

threadAsyncWithUnmask ::
  MonadUnliftIO m =>
  Scope ->
  ((forall x. m x -> m x) -> m a) ->
  m (Thread (Either SomeException a))
threadAsyncWithUnmask =
  unliftedForkWithUnmask threadAsyncWithUnmaskIO
{-# INLINE threadAsyncWithUnmask #-}

threadAsyncWithUnmaskIO :: Scope -> ((forall x. IO x -> IO x) -> IO a) -> IO (Thread (Either SomeException a))
threadAsyncWithUnmaskIO scope action =
  threadAsyncWithRestoreIO scope \restore -> restore (action unsafeUnmask)

threadAsyncWithRestoreIO :: Scope -> ((forall x. IO x -> IO x) -> IO a) -> IO (Thread (Either SomeException a))
threadAsyncWithRestoreIO scope action = do
  parentThreadId <- myThreadId
  resultVar <- newEmptyTMVarIO
  thread'Id <-
    scopeFork scope action \result -> do
      -- FIXME should we put or propagate first?
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

threadAwait :: MonadIO m => Thread a -> m a
threadAwait =
  liftIO . threadAwaitIO
{-# SPECIALIZE threadAwait :: Thread a -> IO a #-}

threadAwaitIO :: Thread a -> IO a
threadAwaitIO thread =
  -- If *they* are deadlocked, we will *both* will be delivered a wakeup from the RTS. We want to shrug this exception
  -- off, because afterwards they'll have put to the result var. But don't shield indefinitely, once will cover this use
  -- case and prevent any accidental infinite loops.
  go `catch` \BlockedIndefinitelyOnSTM -> go
  where
    go =
      atomically (thread'Await thread)

threadAwaitFor :: MonadIO m => Thread a -> Duration -> m (Maybe a)
threadAwaitFor thread duration =
  liftIO (threadAwaitForIO thread duration)
{-# SPECIALIZE threadAwaitFor :: Thread a -> Duration -> IO (Maybe a) #-}

threadAwaitForIO :: Thread a -> Duration -> IO (Maybe a)
threadAwaitForIO thread duration =
  timeoutSTM duration (pure . Just <$> thread'Await thread) (pure Nothing)

threadFork :: MonadUnliftIO m => Scope -> m a -> m (Thread a)
threadFork =
  unliftedFork threadForkIO
{-# INLINE threadFork #-}

threadForkIO :: Scope -> IO a -> IO (Thread a)
threadForkIO scope action =
  threadForkWithRestoreIO scope \restore -> restore action

threadFork_ :: MonadUnliftIO m => Scope -> m () -> m ()
threadFork_ =
  unliftedFork threadForkIO_
{-# INLINE threadFork_ #-}

threadForkIO_ :: Scope -> IO () -> IO ()
threadForkIO_ scope action =
  forkWithRestoreIO_ scope \restore -> restore action

threadForkWithUnmask :: MonadUnliftIO m => Scope -> ((forall x. m x -> m x) -> m a) -> m (Thread a)
threadForkWithUnmask =
  unliftedForkWithUnmask threadForkWithUnmaskIO
{-# INLINE threadForkWithUnmask #-}

threadForkWithUnmaskIO :: Scope -> ((forall x. IO x -> IO x) -> IO a) -> IO (Thread a)
threadForkWithUnmaskIO scope action =
  threadForkWithRestoreIO scope \restore -> restore (action unsafeUnmask)

threadForkWithUnmask_ :: MonadUnliftIO m => Scope -> ((forall x. m x -> m x) -> m ()) -> m ()
threadForkWithUnmask_ =
  unliftedForkWithUnmask threadForkWithUnmaskIO_
{-# INLINE threadForkWithUnmask_ #-}

threadForkWithUnmaskIO_ :: Scope -> ((forall x. IO x -> IO x) -> IO ()) -> IO ()
threadForkWithUnmaskIO_ scope action =
  forkWithRestoreIO_ scope \restore -> restore (action unsafeUnmask)

threadForkWithRestoreIO :: Scope -> ((forall x. IO x -> IO x) -> IO a) -> IO (Thread a)
threadForkWithRestoreIO scope action = do
  parentThreadId <- myThreadId
  resultVar <- newEmptyTMVarIO
  thread'Id <-
    scopeFork scope action \result -> do
      case result of
        Left exception -> maybePropagateException scope parentThreadId exception (const True)
        Right _ -> pure ()
      -- even put async exceptions that we propagated
      -- this isn't totally ideal because a caller awaiting this thread would not be able to distinguish between async
      -- exceptions delivered to this thread, or itself
      putTMVarIO resultVar result
  pure
    Thread
      { thread'Await = readTMVar resultVar >>= either throwSTM pure,
        thread'Id
      }

forkWithRestoreIO_ :: Scope -> ((forall x. IO x -> IO x) -> IO ()) -> IO ()
forkWithRestoreIO_ scope action = do
  parentThreadId <- myThreadId
  _childThreadId <-
    scopeFork scope action \case
      Left exception -> maybePropagateException scope parentThreadId exception (const True)
      Right () -> pure ()
  pure ()

unliftedFork :: MonadUnliftIO m => (Scope -> IO a -> IO b) -> Scope -> m a -> m b
unliftedFork forky scope action =
  withRunInIO \unlift -> forky scope (unlift action)
{-# SPECIALIZE unliftedFork :: (Scope -> IO a -> IO b) -> Scope -> IO a -> IO b #-}

unliftedForkWithUnmask ::
  MonadUnliftIO m =>
  (Scope -> ((forall x. IO x -> IO x) -> IO a) -> IO b) ->
  Scope ->
  ((forall x. m x -> m x) -> m a) ->
  m b
unliftedForkWithUnmask forky scope action =
  withRunInIO \unlift -> forky scope \unmask -> unlift (action (liftIO . unmask . unlift))
{-# SPECIALIZE unliftedForkWithUnmask ::
  (Scope -> ((forall x. IO x -> IO x) -> IO a) -> IO b) ->
  Scope ->
  ((forall x. IO x -> IO x) -> IO a) ->
  IO b
  #-}

maybePropagateException :: Scope -> ThreadId -> SomeException -> (SomeException -> Bool) -> IO ()
maybePropagateException scope parentThreadId exception should =
  whenM shouldPropagateException (throwTo parentThreadId (ThreadFailed exception))
  where
    shouldPropagateException :: IO Bool
    shouldPropagateException
      -- Our scope is (presumably) closing, so don't propagate this exception that (presumably) just came from our
      -- parent. But if our scope's not closed, that means this 'ScopeClosing' definitely came from somewhere else...
      | Just ScopeClosing <- fromException exception = (/= -1) <$> readTVarIO (scope'startingVar scope)
      | otherwise = pure (should exception)
