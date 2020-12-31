{-# LANGUAGE TypeApplications #-}

module Ki.Thread
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

threadAsyncWithRestoreIO :: forall a. Scope -> ((forall x. IO x -> IO x) -> IO a) -> IO (Thread (Either SomeException a))
threadAsyncWithRestoreIO scope action = do
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
