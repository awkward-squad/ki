{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Ki.Scope
  ( Scope,
    cancel,
    cancelledSTM,
    context,
    fork,
    scoped,
    wait,
    waitFor,
    waitSTM,
  )
where

import Control.Exception (AsyncException (ThreadKilled), pattern ErrorCall)
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set
import qualified Ki.AsyncThreadFailed
import Ki.Context (Context)
import qualified Ki.Context
import Ki.Duration (Duration)
import Ki.Prelude
import Ki.Timeout (timeoutSTM)

-- | A __scope__ delimits the lifetime of all __threads__ created within it.
data Scope = Scope
  { context :: Context,
    -- | Whether this scope is closed
    -- Invariant: if closed, no threads are starting
    closedVar :: TVar Bool,
    -- | The set of threads that are currently running
    runningVar :: TVar (Set ThreadId),
    -- | The number of threads that are guaranteed to be just about to start. If this number is non-zero, and that's
    -- problematic (e.g. if we're about to cancel this scope), we always respect it and wait for it to drop to zero
    -- before proceeding.
    startingVar :: TVar Int
  }

-- | /Cancel/ all __contexts__ derived from a __scope__.
cancel :: Scope -> IO ()
cancel Scope {context} =
  Ki.Context.cancel context

cancelledSTM :: Scope -> STM (IO a)
cancelledSTM Scope {context} =
  throwIO <$> Ki.Context.cancelled context

-- | Close a scope, kill all of the running threads, and return the first async exception delivered to us while doing
-- so, if any.
--
-- Preconditions:
--   * The set of threads doesn't include us
--   * We're uninterruptibly masked
close :: Scope -> IO (Maybe SomeException)
close scope@Scope {closedVar, runningVar} = do
  threads <-
    atomically do
      blockUntilNoneStarting scope
      writeTVar closedVar True
      readTVar runningVar
  exception <- killThreads (Set.toList threads)
  atomically (blockUntilNoneRunning scope)
  pure exception

fork :: Scope -> ((forall x. IO x -> IO x) -> IO a) -> (Either SomeException a -> IO ()) -> IO ThreadId
fork scope action k =
  uninterruptibleMask \restore -> do
    atomically (incrementStarting scope)
    childThreadId <-
      forkIO do
        result <- try (action restore)
        k result
        childThreadId <- myThreadId
        atomically (deleteRunning scope childThreadId)
    atomically do
      decrementStarting scope
      insertRunning scope childThreadId
    pure childThreadId

new :: Context -> IO Scope
new parentContext = do
  context <- atomically (Ki.Context.derive parentContext)
  closedVar <- newTVarIO False
  runningVar <- newTVarIO Set.empty
  startingVar <- newTVarIO 0
  pure Scope {context, closedVar, runningVar, startingVar}

scoped :: Context -> (Scope -> IO a) -> IO a
scoped context f = do
  scope <- new context
  uninterruptibleMask \restore -> do
    result <- try (restore (f scope))
    closeScopeException <- close scope
    let throw :: SomeException -> IO a
        throw =
          throwIO . Ki.AsyncThreadFailed.unwrap
    case result of
      -- If the callback failed, we don't care if we were thrown an async exception while closing the scope
      Left exception -> throw exception
      -- Otherwise, throw that exception (if it exists)
      Right value -> do
        whenJust closeScopeException throw
        pure value

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

incrementStarting :: Scope -> STM ()
incrementStarting Scope {closedVar, startingVar} =
  readTVar closedVar >>= \case
    False -> modifyTVar' startingVar (+ 1)
    True -> throwSTM (ErrorCall "ki: scope closed")

decrementStarting :: Scope -> STM ()
decrementStarting Scope {startingVar} =
  modifyTVar' startingVar (subtract 1)

insertRunning :: Scope -> ThreadId -> STM ()
insertRunning Scope {runningVar} threadId =
  modifyTVar' runningVar (Set.insert threadId)

deleteRunning :: Scope -> ThreadId -> STM ()
deleteRunning Scope {runningVar} threadId = do
  running <- readTVar runningVar
  case Set.splitMember threadId running of
    (xs, True, ys) -> writeTVar runningVar $! Set.union xs ys
    _ -> retry

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
      try (unsafeUnmask (throwTo threadId ThreadKilled)) >>= \case
        -- don't drop thread we didn't kill
        Left exception -> loop (acc <> pure exception) (threadId : threadIds)
        Right () -> loop acc threadIds

blockUntilTVar :: TVar a -> (a -> Bool) -> STM ()
blockUntilTVar var f = do
  value <- readTVar var
  unless (f value) retry
