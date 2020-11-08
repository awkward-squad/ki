{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Ki.Scope
  ( Scope (..),
    cancelScope,
    scopeCancelledSTM,
    scopeFork,
    scoped,
    wait,
    waitFor,
    waitSTM,
  )
where

import Control.Exception (fromException, pattern ErrorCall)
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set
import Ki.Context (Context)
import qualified Ki.Context as Context
import Ki.Duration (Duration)
import Ki.Prelude
import Ki.ScopeClosing (ScopeClosing (..))
import Ki.ThreadFailed (ThreadFailedAsync (..))
import Ki.Timeout (timeoutSTM)

-- | A __scope__ delimits the lifetime of all __threads__ created within it.
data Scope = Scope
  { context :: Context,
    -- | Whether this scope is closed.
    -- Invariant: if closed, no threads are starting.
    closedVar :: TVar Bool,
    -- | The set of threads that are currently running.
    runningVar :: TVar (Set ThreadId),
    -- | The number of threads that are *guaranteed* to be about to start, in the sense that only the GHC scheduler can
    -- continue to delay; no async exception can strike here and prevent one of these threads from starting.
    --
    -- If this number is non-zero, and that's problematic (e.g. because we're trying to cancel this scope), we always
    -- respect it and wait for it to drop to zero before proceeding.
    startingVar :: TVar Int
  }

newScope :: Context -> IO Scope
newScope parentContext = do
  context <- atomically (Context.deriveContext parentContext)
  closedVar <- newTVarIO False
  runningVar <- newTVarIO Set.empty
  startingVar <- newTVarIO 0
  pure Scope {context, closedVar, runningVar, startingVar}

-- | /Cancel/ all __contexts__ derived from a __scope__.
cancelScope :: Scope -> IO ()
cancelScope Scope {context} =
  Context.cancelContext context

scopeCancelledSTM :: Scope -> STM (IO a)
scopeCancelledSTM Scope {context} =
  throwIO <$> Context.contextCancelTokenSTM context

-- | Close a scope, kill all of the running threads, and return the first async exception delivered to us while doing
-- so, if any.
--
-- Preconditions:
--   * The set of threads doesn't include us
--   * We're uninterruptibly masked
closeScope :: Scope -> IO (Maybe SomeException)
closeScope scope@Scope {closedVar, runningVar} = do
  threads <-
    atomically do
      blockUntilNoneStarting scope
      writeTVar closedVar True
      readTVar runningVar
  exception <- killThreads (Set.toList threads)
  atomically (blockUntilNoneRunning scope)
  pure exception

scopeFork :: Scope -> ((forall x. IO x -> IO x) -> IO a) -> (ThreadId -> Either SomeException a -> IO ()) -> IO ThreadId
scopeFork Scope {closedVar, runningVar, startingVar} action k =
  uninterruptibleMask \restore -> do
    -- Record the thread as being about to start
    atomically do
      readTVar closedVar >>= \case
        False -> modifyTVar' startingVar (+ 1)
        True -> throwSTM (ErrorCall "ki: scope closed")

    -- Fork the thread
    childThreadId <-
      forkIO do
        childThreadId <- myThreadId
        result <- try (action restore)
        k childThreadId result
        atomically do
          running <- readTVar runningVar
          case Set.splitMember childThreadId running of
            (xs, True, ys) -> writeTVar runningVar $! Set.union xs ys
            _ -> retry

    -- Record the thread as having started
    atomically do
      modifyTVar' startingVar \n -> n -1
      modifyTVar' runningVar (Set.insert childThreadId)

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
    -- If applicable, unwrap the 'AsyncThreadFailed' (assumed to have come from one of our children).
    throw :: SomeException -> IO a
    throw exception =
      case fromException exception of
        Just (ThreadFailedAsync threadFailedException) -> throwIO threadFailedException
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
blockUntilNoneRunning Scope {runningVar} =
  blockUntilTVar runningVar Set.null

blockUntilNoneStarting :: Scope -> STM ()
blockUntilNoneStarting Scope {startingVar} =
  blockUntilTVar startingVar (== 0)

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
