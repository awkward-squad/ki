{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Ki.Internal.Scope
  ( Scope,
    async,
    cancel,
    fork,
    global,
    scoped,
    timeout,
    wait,
  )
where

import Control.Exception (AsyncException (ThreadKilled), Exception (fromException), pattern ErrorCall)
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set
import Ki.Internal.Concurrency
import Ki.Internal.Context (CancelToken (..), Cancelled (..), Context)
import qualified Ki.Internal.Context as Context
import qualified Ki.Internal.Context.Internal
import Ki.Internal.Prelude
import Ki.Internal.Thread (AsyncThreadFailed (..), Thread (Thread), timeout)
import qualified Ki.Internal.Thread as Thread

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
  { context :: Ki.Internal.Context.Internal.Context,
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

async :: forall a. Scope -> (Context => (forall x. IO x -> IO x) -> IO a) -> IO (Thread a)
async scope@Scope {context} action = do
  resultVar <- newEmptyTMVarIO
  uninterruptibleMask \restore -> do
    atomically (incrementStarting scope)
    childThreadId <- forkIO (theThread restore resultVar)
    atomically do
      decrementStarting scope
      insertRunning scope childThreadId
      pure (Thread childThreadId (readTMVar resultVar))
  where
    theThread :: (forall x. IO x -> IO x) -> TMVar (Either SomeException a) -> IO ()
    theThread restore resultVar = do
      result <- try (let ?context = context in action restore)
      childThreadId <- myThreadId
      atomically do
        deleteRunning scope childThreadId
        putTMVar resultVar result

-- | /Cancel/ all __contexts__ derived from a __scope__.
cancel :: Scope -> IO ()
cancel Scope {context} = do
  n <- uniqueInt
  atomically (Context.cancel context (CancelToken n))

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

fork :: Scope -> (Context => (forall x. IO x -> IO x) -> IO ()) -> IO ()
fork scope@Scope {context} action = do
  uninterruptibleMask \restore -> do
    parentThreadId <- myThreadId
    atomically (incrementStarting scope)
    childThreadId <- forkIO (theThread restore parentThreadId)
    atomically do
      decrementStarting scope
      insertRunning scope childThreadId
  where
    theThread :: (forall x. IO x -> IO x) -> ThreadId -> IO ()
    theThread restore parentThreadId = do
      result <- try (let ?context = context in action restore)
      whenLeft result \exception ->
        whenM
          (shouldPropagateException context exception)
          (throwTo parentThreadId (AsyncThreadFailed exception))
      childThreadId <- myThreadId
      atomically (deleteRunning scope childThreadId)

global :: (Context => IO a) -> IO a
global action = do
  context <- Context.new
  let ?context = context in action

inScope :: Scope -> (Context => IO a) -> IO a
inScope Scope {context} action =
  let ?context = context in action

new :: Context => IO Scope
new = do
  context <- atomically (Context.derive ?context)
  closedVar <- newTVarIO False
  runningVar <- newTVarIO Set.empty
  startingVar <- newTVarIO 0
  pure Scope {context, closedVar, runningVar, startingVar}

-- | Perform an action with a new __scope__, then /close/ the __scope__.
--
-- /Throws/:
--
--   * The first exception a __thread__ forked with 'fork' throws, if any.
--
-- ==== __Examples__
--
-- @
-- 'scoped' \\scope -> do
--   'fork' scope worker1
--   'fork' scope worker2
--   'wait' scope
-- @
scoped :: Context => (Context => Scope -> IO a) -> IO a
scoped f = do
  scope <- new
  uninterruptibleMask \restore -> do
    result <- inScope scope (try (restore (f scope)))
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

shouldPropagateException :: Ki.Internal.Context.Internal.Context -> SomeException -> IO Bool
shouldPropagateException context exception =
  case fromException exception of
    Just ThreadKilled -> pure False
    Just _ -> pure True
    Nothing ->
      case fromException exception of
        Just (Cancelled_ token) -> (/= Just token) <$> atomically (Context.cancelled context)
        Nothing -> pure True

blockUntilTVar :: TVar a -> (a -> Bool) -> STM ()
blockUntilTVar var f = do
  value <- readTVar var
  unless (f value) retry
