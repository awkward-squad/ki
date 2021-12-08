{-# LANGUAGE MagicHash #-}

module Ki.Scope
  ( Scope,
    scoped,
    wait,
    --
    Thread,
    await,
    awaitSTM,
    fork,
    forkWith,
    forkWith_,
    fork_,
    forktry,
    forktryWith,
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
import qualified Data.IntMap.Lazy as IntMap
import Data.Void (Void)
import GHC.Conc (enableAllocationLimit, labelThread, setAllocationCounter)
import GHC.IO (unsafeUnmask)
import Ki.Bytes
import Ki.Counter
import Ki.Prelude

------------------------------------------------------------------------------------------------------------------------
-- Scope

-- | A thread scope, which delimits the lifetime of all threads created within it.
data Scope = Scope
  { -- | The set of child threads that are currently running, each keyed by a monotonically increasing int.
    childrenVar :: {-# UNPACK #-} !(TVar (IntMap ThreadId)),
    -- | The counter that holds the (int) key to use for the next child thread.
    nextChildIdCounter :: {-# UNPACK #-} !Counter,
    -- | The number of child threads that are guaranteed to be about to start, in the sense that only the GHC scheduler
    -- can continue to delay; no async exception can strike here and prevent one of these threads from starting.
    --
    -- Sentinel value: -1 means the scope is closed.
    startingVar :: {-# UNPACK #-} !(TVar Int),
    -- | The MVar that a child puts to before propagating the exception to the parent because the delivery is not
    -- guaranteed. This is because the parent must kill its children with asynchronous exceptions uninterruptibly masked
    -- for correctness.
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

-- Trust without verifying that any 'ScopeClosed' exception, which is not exported by this module, was indeed thrown to
-- a thread by this library, and not randomly caught by a user and propagated to some thread.
isScopeClosingException :: SomeException -> Bool
isScopeClosingException exception =
  case fromException exception of
    Just ScopeClosing -> True
    _ -> False

pattern IsScopeClosingException :: SomeException
pattern IsScopeClosingException <- (isScopeClosingException -> True)

-- | Perform an action in a new scope. Just before the action returns, whether with a value or an exception, all living
-- threads created within the scope are killed.
--
-- ==== __Examples__
--
-- @
-- 'Ki.scoped' \\scope -> do
--   thread1 <- 'Ki.fork' scope action1
--   thread2 <- 'Ki.fork' scope action2
--   result1 <- 'Ki.await' thread1
--   result2 <- 'Ki.await' result2
--   pure (result1, result2)
-- @
scoped :: (Scope -> IO a) -> IO a
scoped action = do
  childrenVar <- newTVarIO IntMap.empty
  nextChildIdCounter <- newCounter
  startingVar <- newTVarIO 0
  childExceptionVar <- newEmptyMVar
  let scope = Scope {childrenVar, nextChildIdCounter, startingVar, childExceptionVar}

  uninterruptibleMask \restore -> do
    result <- try (restore (action scope))

    children <-
      atomically do
        -- Block until we haven't committed to starting any threads. Without this, we may create a thread concurrently
        -- with closing its scope, and not grab its thread id to throw an exception to.
        blockUntil0 startingVar
        -- Write the sentinel value indicating that this scope is closed, and it is an error to try to create a thread
        -- within it.
        writeTVar startingVar (-1)
        -- Return the list of currently-running children to kill. Some of them may have *just* started (e.g. if we
        -- initially retried in `blockUntil0` above). That's fine - kill them all!
        readTVar childrenVar

    -- Deliver an async exception to every child in the order they were created.
    traverse_ (flip throwTo ScopeClosing) (IntMap.elems children)

    -- Block until all children have terminated; this relies on children respecting the async exception, which they
    -- must, for correctness. Otherwise, a thread could indeed outlive the scope in which it's created, which is
    -- definitely not structured concurrency!
    atomically (blockUntilEmpty childrenVar)

    -- By now there are three sources of exception:
    --
    --   1) A sync or async exception thrown during the callback, captured in `result`. If applicable, we want to unwrap
    --      the `ThreadFailed` off of this, which was only used to indicate it came from one of our children.
    --   2) A sync or async exception left for us in `childExceptionVar` by a child that tried to propagate it to us
    --      directly, but failed (because we killed it concurrently).
    --   3) An async exception waiting in our exception queue, because we still have async exceptions uninterruptibly
    --      masked.
    --
    -- We cannot throw more than one, so throw them in that priority order.
    case result of
      Left exception -> throwIO (unwrapThreadFailed exception)
      Right value ->
        tryTakeMVar childExceptionVar >>= \case
          Nothing -> pure value
          Just exception -> throwIO exception

-- Spawn a thread in a scope, providing it a function that sets the masking state to the requested masking state. The
-- given action is called with async exceptions at least interruptibly masked, but maybe uninterruptibly, if spawn
-- itself was called with async exceptions uninterruptible masked. The given action must not throw an exception.
spawn :: Scope -> ThreadOpts -> ((forall x. IO x -> IO x) -> IO ()) -> IO ThreadId
spawn
  Scope {childrenVar, nextChildIdCounter, startingVar}
  ThreadOpts {affinity, allocationLimit, label, maskingState = requestedChildMaskingState}
  action = do
    parentMaskingState <- getMaskingState

    let atLeastInterruptiblyMasked :: IO a -> IO a
        atLeastInterruptiblyMasked =
          case parentMaskingState of
            Unmasked -> interruptiblyMasked
            MaskedInterruptible -> id
            MaskedUninterruptible -> id

    -- Interruptible mask is enough because none of the STM operations below block
    atLeastInterruptiblyMasked do
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

          let -- Action that sets the masking state from the current (either MaskedInterruptible or
              -- MaskedUninterruptible) to the requested masking state
              masking :: IO a -> IO a
              masking =
                case requestedChildMaskingState of
                  Unmasked -> unsafeUnmask
                  MaskedInterruptible ->
                    case parentMaskingState of
                      Unmasked -> id
                      MaskedInterruptible -> id
                      MaskedUninterruptible -> interruptiblyMasked -- downgrade
                  MaskedUninterruptible ->
                    case parentMaskingState of
                      Unmasked -> uninterruptiblyMasked
                      MaskedInterruptible -> uninterruptiblyMasked
                      MaskedUninterruptible -> id

          action masking

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
wait :: Scope -> STM ()
wait Scope {childrenVar, startingVar} = do
  blockUntilEmpty childrenVar
  blockUntil0 startingVar

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
data Thread a
  = Thread {-# UNPACK #-} !ThreadId !(STM a)
  deriving stock (Functor)

instance Eq (Thread a) where
  Thread ix _ == Thread iy _ =
    ix == iy

instance Ord (Thread a) where
  compare (Thread ix _) (Thread iy _) =
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
    -- | The maximum amount of bytes a thread may allocate before it is delivered an
    -- 'Control.Exception.AllocationLimitExceeded' exception.
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

unwrapThreadFailed :: SomeException -> SomeException
unwrapThreadFailed e0 =
  case fromException e0 of
    Just (ThreadFailed e1) -> e1
    Nothing -> e0

-- | Create a child thread to perform an action within a scope.
--
-- ==== Exception propagation
--
-- If either
--
-- * The action throws an exception.
-- * The child is thrown an asynchronous exception while performing the action.
--
-- then
--
-- * The child propagates the exception to its parent.
-- * The parent kills all of its other children.
-- * The parent rethrows the exception.
--
-- ==== Masking state
--
-- The child thread is created with asynchronous exceptions unmasked, regardless of the calling thread's masking state.
--
-- To create a child thread with a different initial masking state, use 'Ki.forkWith', and unmask with
-- 'GHC.IO.unsafeUnmask'.
fork :: Scope -> IO a -> IO (Thread a)
fork scope =
  forkWith scope defaultThreadOpts

-- | Variant of 'Ki.fork' for threads that never return.
fork_ :: Scope -> IO Void -> IO ()
fork_ scope =
  forkWith_ scope defaultThreadOpts

-- | Variant of 'Ki.fork' that takes an additional options argument.
forkWith :: Scope -> ThreadOpts -> IO a -> IO (Thread a)
forkWith scope opts action = do
  parentThreadId <- myThreadId
  resultVar <- newEmptyTMVarIO
  ident <-
    spawn scope opts \masking -> do
      result <- try (masking action)
      case result of
        Left exception ->
          when
            (not (isScopeClosingException exception))
            (propagateException parentThreadId exception (childExceptionVar scope))
        Right _ -> pure ()
      -- even put async exceptions that we propagated. this isn't totally ideal because a caller awaiting this thread
      -- would not be able to distinguish between async exceptions delivered to this thread, or itself
      putTMVarIO resultVar result
  pure (Thread ident (readTMVar resultVar >>= either throwSTM pure))

-- | Variant of 'Ki.forkWith' for threads that should run until they are killed.
forkWith_ :: Scope -> ThreadOpts -> IO Void -> IO ()
forkWith_ scope opts action = do
  parentThreadId <- myThreadId
  _childThreadId <-
    spawn scope opts \masking ->
      tryEither
        ( \exception ->
            when
              (not (isScopeClosingException exception))
              (propagateException parentThreadId exception (childExceptionVar scope))
        )
        (\_unit -> pure ())
        (masking action)
  pure ()

-- | Like 'Ki.fork', but if the action throws an exception that is an instance of the given exception type, then it is
-- returned rather than propagated to the child's parent.
--
-- For example, @forktry \@SomeException@ creates a thread that does not propagate any synchronous exceptions, because
-- every exception is an instance of 'Control.Exception.SomeException'.
--
-- It is not possible to prevent propagating asynchronous exceptions.
forktry :: forall e a. Exception e => Scope -> IO a -> IO (Thread (Either e a))
forktry scope =
  forktryWith scope defaultThreadOpts

-- | Variant of 'Ki.forktry' that takes an additional options argument.
forktryWith :: forall e a. Exception e => Scope -> ThreadOpts -> IO a -> IO (Thread (Either e a))
forktryWith scope opts action = do
  parentThreadId <- myThreadId
  resultVar <- newEmptyTMVarIO
  ident <-
    spawn scope opts \masking -> do
      result <- try (masking action)
      case result of
        Left exception -> do
          let shouldPropagate =
                case fromException @e exception of
                  Nothing -> not (isScopeClosingException exception)
                  -- if the user calls `forktry @MyAsyncException`, we still want to propagate the async exception
                  Just _ -> not (isScopeClosingException exception) && isAsyncException exception
          when shouldPropagate (propagateException parentThreadId exception (childExceptionVar scope))
        Right _value -> pure ()
      putTMVarIO resultVar result
  let doAwait =
        readTMVar resultVar >>= \case
          Left exception ->
            case fromException @e exception of
              Nothing -> throwSTM exception
              Just exception' -> pure (Left exception')
          Right value -> pure (Right value)
  pure (Thread ident doAwait)
  where
    isAsyncException :: SomeException -> Bool
    isAsyncException exception =
      case fromException @SomeAsyncException exception of
        Nothing -> False
        Just _ -> True

-- | Wait for a thread to terminate, and return its value.
await :: Thread a -> IO a
await thread =
  -- If *they* are deadlocked, we will *both* will be delivered a wakeup from the RTS. We want to shrug this exception
  -- off, because afterwards they'll have put to the result var. But don't shield indefinitely, once will cover this use
  -- case and prevent any accidental infinite loops.
  tryEither (\BlockedIndefinitelyOnSTM -> go) pure go
  where
    go =
      atomically (awaitSTM thread)

-- | @STM@ variant of 'Ki.await'.
awaitSTM :: Thread a -> STM a
awaitSTM (Thread _threadId doAwait) =
  doAwait

-- TODO more docs
-- No precondition on masking state
propagateException :: ThreadId -> SomeException -> MVar SomeException -> IO ()
propagateException parentThreadId exception childExceptionVar =
  -- `throwTo` cannot be called with async exceptions uninterruptibly masked, because that may deadlock with our parent
  -- throwing to us
  interruptiblyMasked do
    fix \again ->
      try (throwTo parentThreadId (ThreadFailed exception)) >>= \case
        Left IsScopeClosingException -> void (tryPutMVar childExceptionVar exception)
        -- while blocking on notifying the parent of this exception, we got hit by a random async exception from
        -- elsewhere. that's weird and unexpected, but we already have an exception to deliver, so it just gets tossed
        -- to the void...
        Left _ -> again
        Right _ -> pure ()

-- Like try, but with continuations
tryEither :: Exception e => (e -> IO b) -> (a -> IO b) -> IO a -> IO b
tryEither onFailure onSuccess action =
  join (catch (onSuccess <$> action) (pure . onFailure))
