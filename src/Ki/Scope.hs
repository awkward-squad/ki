{-# LANGUAGE PatternSynonyms #-}

module Ki.Scope
  ( Scope (..),
    cancel,
    scopeCancelledSTM,
    scopeFork,
    scoped,
    wait,
    waitFor,
    waitSTM,
    ScopeClosing (..),
    ThreadFailed (..),
  )
where

import Control.Exception
  ( Exception (fromException, toException),
    asyncExceptionFromException,
    asyncExceptionToException,
    pattern ErrorCall,
  )
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set
import Ki.CancelToken
import Ki.Context
import Ki.Duration (Duration)
import Ki.Prelude
import Ki.Timeout (timeoutSTM)

-- | A __scope__ delimits the lifetime of all __threads__ created within it.
data Scope = Scope
  { scope'context :: Context,
    -- | The set of threads that are currently running.
    scope'runningVar :: TVar (Set ThreadId),
    -- | The number of threads that are *guaranteed* to be about to start, in the sense that only the GHC scheduler can
    -- continue to delay; no async exception can strike here and prevent one of these threads from starting.
    --
    -- If this number is non-zero, and that's problematic (e.g. because we're trying to cancel this scope), we always
    -- respect it and wait for it to drop to zero before proceeding.
    --
    -- Sentinel value: -1 means the scope is closed.
    scope'startingVar :: TVar Int
  }

newScope :: Context -> IO Scope
newScope parentContext =
  Scope
    <$> deriveContext parentContext
    <*> newTVarIO Set.empty
    <*> newTVarIO 0

-- | /Cancel/ all __contexts__ derived from a __scope__.
cancel :: Scope -> IO ()
cancel scope = do
  token <- newCancelToken
  atomically (cancelContext (scope'context scope) token)

scopeCancelledSTM :: Scope -> STM (IO a)
scopeCancelledSTM scope =
  throwIO <$> contextCancelToken (scope'context scope)

-- | Close a scope, kill all of the running threads, and return the first async exception delivered to us while doing
-- so, if any.
--
-- Preconditions:
--   * The set of threads doesn't include us
--   * We're uninterruptibly masked
closeScope :: Scope -> IO (Maybe SomeException)
closeScope scope = do
  threads <-
    atomically do
      blockUntilNoneStarting scope
      writeTVar (scope'startingVar scope) (-1)
      readTVar (scope'runningVar scope)
  exception <- killThreads (Set.toList threads)
  atomically do
    blockUntilNoneRunning scope
    readTVar (context'cancelStateVar (scope'context scope)) >>= \case
      CancelState'NotCancelled -> context'removeFromParent (scope'context scope)
      CancelState'Cancelled _token _way -> pure ()
  pure exception

scopeFork :: Scope -> ((forall x. IO x -> IO x) -> IO a) -> (Either SomeException a -> IO ()) -> IO ThreadId
scopeFork scope action k =
  uninterruptibleMask \restore -> do
    -- Record the thread as being about to start
    atomically do
      starting <- readTVar (scope'startingVar scope)
      if starting == -1
        then throwSTM (ErrorCall "ki: scope closed")
        else writeTVar (scope'startingVar scope) $! starting + 1

    -- Fork the thread
    childThreadId <-
      forkIO do
        childThreadId <- myThreadId
        result <- try (action restore)
        k result
        atomically do
          running <- readTVar (scope'runningVar scope)
          -- Why not just delete? We wouldn't want to delete *nothing*, *then* insert from the parent thread.
          case Set.splitMember childThreadId running of
            (xs, True, ys) -> writeTVar (scope'runningVar scope) $! Set.union xs ys
            _ -> retry

    -- Record the thread as having started
    atomically do
      modifyTVar' (scope'startingVar scope) \n -> n -1
      modifyTVar' (scope'runningVar scope) (Set.insert childThreadId)

    pure childThreadId

scoped :: Context -> (Scope -> IO a) -> IO a
scoped context f = do
  scope <- newScope context
  uninterruptibleMask \restore -> do
    result <- try (restore (f scope))
    closeScopeException <- closeScope scope
    -- If the callback failed, we don't care if we were thrown an async exception while closing the scope.
    -- Otherwise, throw that exception (if it exists).
    case result of
      Left exception -> throw exception
      Right value -> do
        whenJust closeScopeException throw
        pure value
  where
    -- If applicable, unwrap the 'ThreadFailed' (assumed to have come from one of our children).
    throw :: SomeException -> IO a
    throw exception =
      case fromException exception of
        Just (ThreadFailed threadFailedException) -> throwIO threadFailedException
        Nothing -> throwIO exception

-- | Wait until all __threads__ created within a __scope__ finish.
wait :: Scope -> IO ()
wait =
  atomically . waitSTM

-- | Variant of 'wait' that waits for up to the given duration.
waitFor :: Scope -> Duration -> IO ()
waitFor scope duration =
  timeoutSTM duration (pure <$> waitSTM scope) (pure ())

-- | @STM@ variant of 'wait'.
waitSTM :: Scope -> STM ()
waitSTM scope = do
  blockUntilNoneRunning scope
  blockUntilNoneStarting scope

--------------------------------------------------------------------------------
-- Scope helpers

blockUntilNoneRunning :: Scope -> STM ()
blockUntilNoneRunning scope =
  blockUntilTVar (scope'runningVar scope) Set.null

blockUntilNoneStarting :: Scope -> STM ()
blockUntilNoneStarting scope =
  blockUntilTVar (scope'startingVar scope) (== 0)

--------------------------------------------------------------------------------
-- Internal exception types

-- | Exception thrown by a parent __thread__ to its children when the __scope__ is closing.
data ScopeClosing
  = ScopeClosing
  deriving stock (Eq, Show)

instance Exception ScopeClosing where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

-- | Exception thrown by a child __thread__ to its parent, if it fails unexpectedly.
newtype ThreadFailed
  = ThreadFailed SomeException
  deriving stock (Show)

instance Exception ThreadFailed where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

--------------------------------------------------------------------------------
-- Misc. utils

killThreads :: [ThreadId] -> IO (Maybe SomeException)
killThreads =
  (`fix` mempty) \loop acc -> \case
    [] -> pure (Monoid.getFirst acc)
    threadId : threadIds ->
      -- We unmask because we don't want to deadlock with a thread
      -- that is concurrently trying to throw an exception to us with
      -- exceptions masked.
      try (unsafeUnmask (throwTo threadId ScopeClosing)) >>= \case
        -- don't drop thread we didn't (necessarily) deliver the exception to
        Left exception -> loop (acc <> Monoid.First (Just exception)) (threadId : threadIds)
        Right () -> loop acc threadIds

blockUntilTVar :: TVar a -> (a -> Bool) -> STM ()
blockUntilTVar var f = do
  value <- readTVar var
  unless (f value) retry
