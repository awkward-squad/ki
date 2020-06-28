{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Ki.Scope.Internal
  ( Scope,
    async,
    cancel,
    fork,
    scoped,
    wait,
    withContext,
  )
where

import Control.Exception (AsyncException (ThreadKilled), Exception (fromException), pattern ErrorCall)
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set
import Ki.Concurrency
import Ki.Context.Internal (Context)
import qualified Ki.Context.Internal as Context
import Ki.Prelude
import Ki.Thread (AsyncThreadFailed (..), Thread (Thread))
import qualified Ki.Thread as Thread

-- | A __scope__ delimits the lifetime of all __threads__ forked within it. A __thread__ cannot outlive its __scope__.
--
-- When a __scope__ is /closed/, all remaining __threads__ forked within it are killed.
--
-- The basic usage of a __scope__ is as follows.
--
-- @
-- 'scoped' \\scope -> do
--   'fork' scope worker1
--   'fork' scope worker2
--   'wait' scope
-- @
--
-- A __scope__ can be passed into functions or shared amongst __threads__, but this is generally not advised, as it
-- takes the "structure" out of "structured concurrency".
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

async :: forall a. Scope -> ((forall x. IO x -> IO x) -> IO a) -> IO (Thread a)
async scope action = do
  resultVar <- newEmptyTMVarIO
  childThreadId <-
    _fork scope action \result ->
      atomically (putTMVar resultVar result)
  pure (Thread childThreadId (readTMVar resultVar))

-- | /Cancel/ all __contexts__ derived from a __scope__.
cancel :: Scope -> IO ()
cancel Scope {context} = do
  Context.cancel context

-- | Close a scope, kill all of the running threads, and return the first
-- async exception delivered to us while doing so, if any.
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
  pure (Monoid.getFirst exception)

fork :: Scope -> ((forall x. IO x -> IO x) -> IO ()) -> IO ()
fork scope@Scope {context} action = do
  parentThreadId <- myThreadId
  _ <-
    _fork scope action \result ->
      whenLeft result \exception ->
        whenM
          (shouldPropagateException context exception)
          (throwTo parentThreadId (AsyncThreadFailed exception))
  pure ()

_fork :: Scope -> ((forall x. IO x -> IO x) -> IO a) -> (Either SomeException a -> IO ()) -> IO ThreadId
_fork scope action k =
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
  context <- atomically (Context.derive parentContext)
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
    case result of
      -- If the callback failed, we don't care if we were thrown an async exception while closing the scope
      Left exception -> throwIO (Thread.unwrapAsyncThreadFailed exception)
      -- Otherwise, throw that exception (if it exists)
      Right value -> do
        for_ @Maybe closeScopeException (throwIO . Thread.unwrapAsyncThreadFailed)
        pure value

wait :: Scope -> STM ()
wait scope = do
  blockUntilNoneRunning scope
  blockUntilNoneStarting scope

withContext :: Scope -> (Context -> a) -> a
withContext Scope {context} action =
  action context

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

killThreads :: [ThreadId] -> IO (Monoid.First SomeException)
killThreads =
  loop mempty
  where
    loop :: Monoid.First SomeException -> [ThreadId] -> IO (Monoid.First SomeException)
    loop acc = \case
      [] -> pure acc
      threadId : threadIds ->
        -- We unmask because we don't want to deadlock with a thread
        -- that is concurrently trying to throw an exception to us with
        -- exceptions masked.
        try (unsafeUnmask (throwTo threadId ThreadKilled)) >>= \case
          -- don't drop thread we didn't kill
          Left exception -> loop (acc <> pure exception) (threadId : threadIds)
          Right () -> loop acc threadIds

shouldPropagateException :: Context -> SomeException -> IO Bool
shouldPropagateException context exception =
  case fromException exception of
    Just ThreadKilled -> pure False
    Just _ -> pure True
    Nothing -> not <$> atomically (Context.matchCancelled context exception)

blockUntilTVar :: TVar a -> (a -> Bool) -> STM ()
blockUntilTVar var f = do
  value <- readTVar var
  unless (f value) retry
