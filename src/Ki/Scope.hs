{-# LANGUAGE MagicHash #-}

module Ki.Scope
  ( Scope,
    scoped,
    wait,
    waitFor,
    waitSTM,
    --
    Thread,
    async,
    asyncWith,
    await,
    awaitFor,
    awaitSTM,
    fork,
    fork_,
    forkWith,
    forkWith_,
    --
    ThreadAffinity (..),
    ThreadOpts (..),
    defaultThreadOpts,
  )
where

import Control.Exception
  ( BlockedIndefinitelyOnSTM (..),
    Exception (fromException, toException),
    MaskingState (..),
    SomeAsyncException,
    asyncExceptionFromException,
    asyncExceptionToException,
    catch,
    getMaskingState,
    pattern ErrorCall,
  )
import Control.Monad.IO.Unlift (MonadUnliftIO (withRunInIO))
import qualified Data.IntMap.Lazy as IntMap
import GHC.Conc (enableAllocationLimit, labelThread, setAllocationCounter)
import GHC.IO (unsafeUnmask)
import Ki.Bytes
import Ki.Counter
import Ki.Duration (Duration)
import Ki.Prelude
import Ki.Timeout

------------------------------------------------------------------------------------------------------------------------
-- Scope

-- | A scope delimits the lifetime of all threads created within it.
data Scope = Scope
  { -- | The set of child threads that are currently running, each keyed by a monotonically increasing int.
    childrenVar :: {-# UNPACK #-} !(TVar (IntMap ThreadId)),
    -- | The counter that holds the (int) key to use for the next child thread.
    nextChildIdCounter :: {-# UNPACK #-} !Counter,
    -- | The number of child threads that are guaranteed to be about to start, in the sense that only the GHC scheduler can continue to
    -- delay; no async exception can strike here and prevent one of these threads from starting.
    --
    -- Sentinel value: -1 means the scope is closed.
    startingVar :: {-# UNPACK #-} !(TVar Int),
    -- | The MVar that a child puts to before propagating the exception to the parent because the delivery is not guaranteed. This is
    -- because the parent must kill its children with asynchronous exceptions uninterruptibly masked for correctness.
    childExceptionVar :: {-# UNPACK #-} !(MVar SomeException)
  }

-- Exception thrown by a parent thread to its children when its scope is closing.
data ScopeClosing
  = ScopeClosing

instance Show ScopeClosing where
  show _ = "ScopeClosing"

instance Exception ScopeClosing where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

-- | Perform an action in a new scope. When the action returns, all living threads created within the scope are killed.
--
-- ==== __Examples__
--
-- @
-- 'Ki.scoped' \\scope -> do
--   'Ki.fork_' scope worker1
--   'Ki.fork_' scope worker2
--   'Ki.wait' scope
-- @
scoped :: MonadUnliftIO m => (Scope -> m a) -> m a
scoped action =
  withRunInIO \unlift -> do
    childrenVar <- newTVarIO IntMap.empty
    nextChildIdCounter <- newCounter
    startingVar <- newTVarIO 0
    childExceptionVar <- newEmptyMVar
    let scope = Scope {childrenVar, nextChildIdCounter, startingVar, childExceptionVar}

    uninterruptibleMask \restore -> do
      result <- try (restore (unlift (action scope)))

      children <-
        atomically do
          -- Block until we haven't committed to starting any threads. Without this, we may create a thread concurrently with closing its
          -- scope, and not grab its thread id to throw an exception to.
          blockUntil0 startingVar
          -- Write the sentinel value indicating that this scope is closed, and it is an error to try to create a thread within it.
          writeTVar startingVar (-1)
          -- Return the list of currently-running children to kill. Some of them may have *just* started (e.g. if we initially retried in
          -- `blockUntil0` above). That's fine - kill them all!
          readTVar childrenVar

      -- Deliver an async exception to every child in the order they were created.
      traverse_ (flip throwTo ScopeClosing) (IntMap.elems children)

      -- Block until all children have terminated; this relies on children respecting the async exception, which they must, for correctness.
      -- Otherwise, a thread could indeed outlive the scope in which it's created, which is definitely not structured concurrency!
      atomically (blockUntilEmpty childrenVar)

      -- If the callback failed, we don't care if we were thrown an async exception while closing the scope. Otherwise, throw that exception
      -- (if it exists).
      case result of
        -- If applicable, unwrap the 'ThreadFailed' (assumed to have come from one of our children).
        Left exception -> case fromException exception of
          Just (ThreadFailed threadFailedException) -> throwIO threadFailedException
          Nothing -> throwIO exception
        Right value -> do
          tryTakeMVar childExceptionVar >>= \case
            Nothing -> pure value
            Just ex -> throwIO ex
{-# INLINE scoped #-}
{-# SPECIALIZE scoped :: (Scope -> IO a) -> IO a #-}

-- Spawn a thread in a scope, providing it a function that sets the masking state to the requested masking state. The given action must not
-- throw an exception.
spawn :: Scope -> ThreadOpts -> ((forall x. IO x -> IO x) -> IO ()) -> IO ThreadId
spawn
  Scope {childrenVar, nextChildIdCounter, startingVar}
  ThreadOpts {affinity, allocationLimit, label, maskingState = requestedChildMaskingState}
  action = do
    parentMaskingState <- getMaskingState

    let atLeastBlockI :: IO a -> IO a
        atLeastBlockI =
          case parentMaskingState of
            Unmasked -> blockI
            MaskedInterruptible -> id
            MaskedUninterruptible -> id

    atLeastBlockI do
      -- Record the thread as being about to start.
      atomically do
        readTVar startingVar >>= \case
          -1 -> throwSTM (ErrorCall "ki: scope closed")
          n -> writeTVar startingVar $! n + 1

      childId <- incrCounter nextChildIdCounter

      childThreadId <-
        forkWithAffinity affinity do
          when (not (null label)) do
            childThreadId <- myThreadId
            labelThread childThreadId label

          whenJust allocationLimit \bytes -> do
            setAllocationCounter (bytesToInt64 bytes)
            enableAllocationLimit

          action case requestedChildMaskingState of
            Unmasked -> unsafeUnmask
            MaskedInterruptible ->
              case parentMaskingState of
                Unmasked -> id
                MaskedInterruptible -> id
                MaskedUninterruptible -> blockI -- downgrade
            MaskedUninterruptible ->
              case parentMaskingState of
                Unmasked -> blockU
                MaskedInterruptible -> blockU
                MaskedUninterruptible -> id

          atomically (unrecordChild childrenVar childId)

      -- Record the child as having started
      atomically do
        modifyTVar' startingVar \n -> n -1
        recordChild childrenVar childId childThreadId

      pure childThreadId

-- Record our child by either:
--
--   * Flipping `Nothing` to `Just childThreadId` (common case: we record child before it unrecords itself)
--   * Flipping `Just _` to `Nothing` (uncommon case: we observe that a child already unrecorded itself)
--
-- Never retries.
recordChild :: TVar (IntMap ThreadId) -> Int -> ThreadId -> STM ()
recordChild childrenVar childId childThreadId =
  modifyTVar' childrenVar (IntMap.alter (maybe (Just childThreadId) (const Nothing)) childId)

-- Unrecord a child (ourselves) by either:
--
--   * Flipping `Just childThreadId` to `Nothing` (common case: parent recorded us first)
--   * Flipping `Nothing` to `Just undefined` (uncommon case: we die and unrecord before parent can record us).
--
-- Never retries.
unrecordChild :: TVar (IntMap ThreadId) -> Int -> STM ()
unrecordChild childrenVar childId =
  modifyTVar' childrenVar (IntMap.alter (maybe (Just undefined) (const Nothing)) childId)

-- forkIO/forkOn/forkOS, switching on affinity
forkWithAffinity :: Maybe ThreadAffinity -> IO () -> IO ThreadId
forkWithAffinity = \case
  Nothing -> forkIO
  Just (Capability n) -> forkOn n
  Just OsThread -> forkOS

-- | Wait until all threads created within a scope terminate.
wait :: MonadIO m => Scope -> m ()
wait =
  liftIO . atomically . waitSTM
{-# INLINE wait #-}
{-# SPECIALIZE wait :: Scope -> IO () #-}

-- | Variant of 'Ki.wait' that waits for up to the given duration.
waitFor :: MonadIO m => Scope -> Duration -> m ()
waitFor scope duration =
  liftIO (timeoutSTM duration (pure <$> waitSTM scope) (pure ()))
{-# INLINE waitFor #-}
{-# SPECIALIZE waitFor :: Scope -> Duration -> IO () #-}

-- | @STM@ variant of 'Ki.wait'.
waitSTM :: Scope -> STM ()
waitSTM Scope {childrenVar, startingVar} = do
  blockUntilEmpty childrenVar
  blockUntil0 startingVar
{-# INLINE waitSTM #-}

-- Block until an IntMap becomes empty.
blockUntilEmpty :: TVar (IntMap a) -> STM ()
blockUntilEmpty var = do
  x <- readTVar var
  when (not (IntMap.null x)) retry

-- Block until a TVar becomes 0.
blockUntil0 :: TVar Int -> STM ()
blockUntil0 var =
  readTVar var >>= \case
    0 -> pure ()
    _ -> retry

------------------------------------------------------------------------------------------------------------------------
-- Thread

-- | A thread created within a scope.
data Thread a = Thread
  { await_ :: !(STM a),
    ident :: {-# UNPACK #-} !ThreadId
  }
  deriving stock (Functor)

instance Eq (Thread a) where
  Thread _ ix == Thread _ iy =
    ix == iy

instance Ord (Thread a) where
  compare (Thread _ ix) (Thread _ iy) =
    compare ix iy

data ThreadAffinity
  = -- | Bound to a capability.
    Capability Int
  | -- | Bound to an OS thread.
    OsThread
  deriving stock (Eq, Show)

-- | Thread options that can be provided at the time a thread is created.
data ThreadOpts = ThreadOpts
  { affinity :: Maybe ThreadAffinity,
    -- | The maximum amount of bytes a thread may allocate before it is delivered an 'AllocationLimitExceeded'
    -- exception.
    allocationLimit :: Maybe Bytes,
    label :: String,
    -- | The masking state a thread is created in.
    maskingState :: MaskingState
  }
  deriving stock (Eq, Show)

-- |
-- @
-- ThreadOpts
--   { affinity = Nothing
--   , allocationLimit = Nothing
--   , label = ""
--   , maskingState = Unmasked
--   }
-- @
defaultThreadOpts :: ThreadOpts
defaultThreadOpts =
  ThreadOpts
    { affinity = Nothing,
      allocationLimit = Nothing,
      label = "",
      maskingState = Unmasked
    }

-- Internal exception type thrown by a child thread to its parent, if it fails unexpectedly.
newtype ThreadFailed
  = ThreadFailed SomeException
  deriving stock (Show)

instance Exception ThreadFailed where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

-- | Create a thread within a scope.
--
-- The thread is created with asynchronous exceptions unmasked.
async :: MonadUnliftIO m => Scope -> m a -> m (Thread (Either SomeException a))
async scope =
  asyncWith scope defaultThreadOpts
{-# INLINE async #-}
{-# SPECIALIZE async :: Scope -> IO a -> IO (Thread (Either SomeException a)) #-}

-- | Variant of 'Ki.async' that takes an additional options argument.
asyncWith :: MonadUnliftIO m => Scope -> ThreadOpts -> m a -> m (Thread (Either SomeException a))
asyncWith scope opts action =
  withRunInIO \unlift -> do
    parentThreadId <- myThreadId
    resultVar <- newEmptyTMVarIO
    ident <-
      spawn scope opts \masking -> do
        result <- try (masking (unlift action))
        case result of
          Left exception -> maybePropagateException parentThreadId exception isAsyncException (childExceptionVar scope)
          Right _ -> pure ()
        putTMVarIO resultVar result -- even put async exceptions that we propagated
    pure
      Thread
        { await_ = readTMVar resultVar,
          ident
        }
  where
    isAsyncException :: SomeException -> Bool
    isAsyncException exception =
      case fromException @SomeAsyncException exception of
        Nothing -> False
        Just _ -> True
{-# INLINE asyncWith #-}
{-# SPECIALIZE asyncWith :: Scope -> ThreadOpts -> IO a -> IO (Thread (Either SomeException a)) #-}

-- | Wait for a thread to terminate, and return its value.
await :: MonadIO m => Thread a -> m a
await thread =
  -- If *they* are deadlocked, we will *both* will be delivered a wakeup from the RTS. We want to shrug this exception off, because
  -- afterwards they'll have put to the result var. But don't shield indefinitely, once will cover this use case and prevent any accidental
  -- infinite loops.
  liftIO do
    join do
      catch
        ( do
            result <- go
            pure (pure result)
        )
        \BlockedIndefinitelyOnSTM -> pure go
  where
    go =
      atomically (await_ thread)
{-# INLINE await #-}
{-# SPECIALIZE await :: Thread a -> IO a #-}

-- | Variant of 'Ki.await' that gives up after the given duration.
awaitFor :: MonadIO m => Thread a -> Duration -> m (Maybe a)
awaitFor thread duration =
  liftIO (timeoutSTM duration (pure . Just <$> await_ thread) (pure Nothing))
{-# INLINE awaitFor #-}
{-# SPECIALIZE awaitFor :: Thread a -> Duration -> IO (Maybe a) #-}

-- | @STM@ variant of 'Ki.await'.
awaitSTM :: Thread a -> STM a
awaitSTM =
  await_

-- | Create a thread within a scope.
--
-- The thread is created with asynchronous exceptions unmasked. If the thread terminates with an exception, the exception is propagated to
-- the thread's parent.
fork :: MonadUnliftIO m => Scope -> m a -> m (Thread a)
fork scope =
  forkWith scope defaultThreadOpts
{-# INLINE fork #-}
{-# SPECIALIZE fork :: Scope -> IO a -> IO (Thread a) #-}

-- | Variant of 'Ki.fork' that does not return the thread.
fork_ :: MonadUnliftIO m => Scope -> m () -> m ()
fork_ scope =
  forkWith_ scope defaultThreadOpts
{-# INLINE fork_ #-}
{-# SPECIALIZE fork_ :: Scope -> IO () -> IO () #-}

-- | Variant of 'Ki.fork' that takes an additional options argument.
forkWith :: MonadUnliftIO m => Scope -> ThreadOpts -> m a -> m (Thread a)
forkWith scope opts action =
  withRunInIO \unlift -> do
    parentThreadId <- myThreadId
    resultVar <- newEmptyTMVarIO
    ident <-
      spawn scope opts \masking -> do
        result <- try (masking (unlift action))
        case result of
          Left exception -> maybePropagateException parentThreadId exception (const True) (childExceptionVar scope)
          Right _ -> pure ()
        -- even put async exceptions that we propagated. this isn't totally ideal because a caller awaiting this thread would not be able to
        -- distinguish between async exceptions delivered to this thread, or itself
        putTMVarIO resultVar result
    pure
      Thread
        { await_ = readTMVar resultVar >>= either throwSTM pure,
          ident
        }
{-# SPECIALIZE forkWith :: Scope -> ThreadOpts -> IO a -> IO (Thread a) #-}

-- | Variant of 'Ki.forkWith' that does not return the thread.
forkWith_ :: MonadUnliftIO m => Scope -> ThreadOpts -> m () -> m ()
forkWith_ scope opts action =
  withRunInIO \unlift -> do
    parentThreadId <- myThreadId
    _childThreadId <-
      spawn scope opts \masking ->
        join do
          catch
            ( do
                masking (unlift action)
                pure (pure ())
            )
            \exception -> pure (maybePropagateException parentThreadId exception (const True) (childExceptionVar scope))
    pure ()
{-# SPECIALIZE forkWith_ :: Scope -> ThreadOpts -> IO () -> IO () #-}

maybePropagateException :: ThreadId -> SomeException -> (SomeException -> Bool) -> MVar SomeException -> IO ()
maybePropagateException parentThreadId exception should childExceptionVar =
  when shouldPropagateException do
    let go =
          try (unsafeUnmask (throwTo parentThreadId (ThreadFailed exception))) >>= \case
            Left e -> case fromException e of
              Just ScopeClosing -> void (tryPutMVar childExceptionVar exception)
              _ -> go
            Right _ -> pure ()
    go
  where
    shouldPropagateException :: Bool
    shouldPropagateException
      -- Trust without verifying that any 'ScopeClosed' exception, which is not exported by this module, was indeed thrown to a thread by
      -- this library, and not randomly caught by a user and propagated to some thread.
      | Just ScopeClosing <- fromException exception = False
      | otherwise = should exception
