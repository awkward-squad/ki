{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Ki.Indef.Scope
  ( Scope,
    async,
    fork,
    cancel,
    scoped,
    timeout,
    wait,
  )
where

import Control.Exception (AsyncException (ThreadKilled), Exception (fromException), SomeException, pattern ErrorCall)
import Control.Monad (unless)
import Data.Coerce (coerce)
import Data.Foldable (for_)
import qualified Data.Monoid as Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Ki.Indef.Context (CancelToken (..), Cancelled (..), Context)
import qualified Ki.Indef.Context as Ki.Context
import Ki.Indef.Thread (AsyncThreadFailed (..), Thread (Thread), timeout)
import qualified Ki.Indef.Thread as Thread
import Ki.Sig (IO, STM, TMVar, TVar, ThreadId, atomically, forkIO, modifyTVar', myThreadId, newEmptyTMVarIO, newTVar, newUnique, putTMVar, readTVar, retry, throwIO, throwSTM, throwTo, try, uninterruptibleMask, unsafeUnmask, writeTVar)
import Prelude hiding (IO)

-- import Ki.Internal.Debug

data Scope = Scope
  { context :: Context,
    -- | Whether this scope is closed
    -- Invariant: if closed, no threads are starting
    closedVar :: TVar Bool,
    runningVar :: TVar (Set ThreadId),
    -- | The number of threads that are just about to start
    startingVar :: TVar Int
  }

async :: forall a. Scope -> (Context -> (forall x. IO x -> IO x) -> IO a) -> IO (Thread a)
async scope@Scope {context} action = do
  uninterruptibleMask \restore -> do
    resultVar <- newEmptyTMVarIO "result"
    atomically (incrementStarting scope)
    childThreadId <- forkIO (theThread restore resultVar)
    atomically do
      decrementStarting scope
      insertRunning scope childThreadId
      pure (Thread childThreadId resultVar)
  where
    theThread :: (forall x. IO x -> IO x) -> TMVar (Either SomeException a) -> IO ()
    theThread restore resultVar = do
      result <- try (action context restore)
      childThreadId <- myThreadId
      atomically do
        deleteRunning scope childThreadId
        putTMVar resultVar result

cancel :: Scope -> IO ()
cancel Scope {context} = do
  token <- newUnique
  atomically (Ki.Context.cancel context (CancelToken token))

fork :: Scope -> (Context -> (forall x. IO x -> IO x) -> IO a) -> IO ()
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
      result <- try (action context restore)
      whenLeft result \exception ->
        whenM
          (shouldPropagateException context exception)
          (throwTo parentThreadId (AsyncThreadFailed exception))
      childThreadId <- myThreadId
      atomically (deleteRunning scope childThreadId)

scoped :: Context -> (Scope -> IO a) -> IO a
scoped parentContext f = do
  uninterruptibleMask \restore -> do
    scope <- new
    result <- restore (try (f scope))
    closeScopeException <- closeScope scope
    case result of
      -- If the callback failed, we don't care if we were thrown an async
      -- exception while closing the scope
      Left exception -> throwIO (Thread.unwrapAsyncThreadFailed exception)
      -- Otherwise, throw that exception (if it exists)
      Right value -> do
        for_ @Maybe closeScopeException throwIO
        pure value
  where
    new :: IO Scope
    new =
      atomically do
        context <- Ki.Context.derive parentContext
        closedVar <- newTVar "closed" False
        runningVar <- newTVar "running" Set.empty
        startingVar <- newTVar "starting" 0
        pure Scope {context, closedVar, runningVar, startingVar}

    -- Close a scope, kill all of the running threads, and return the first
    -- async exception delivered to us while doing so, if any.
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
      pure (coerce @(Monoid.First SomeException) @(Maybe SomeException) exception)
      where
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

shouldPropagateException :: Context -> SomeException -> IO Bool
shouldPropagateException context exception =
  case fromException exception of
    Just ThreadKilled -> pure False
    Just _ -> pure True
    Nothing ->
      case fromException exception of
        Just (Cancelled_ token) -> (/= Just token) <$> Ki.Context.cancelled context
        Nothing -> pure True

blockUntilTVar :: TVar a -> (a -> Bool) -> STM ()
blockUntilTVar var f = do
  value <- readTVar var
  unless (f value) retry

whenLeft :: Applicative m => Either a b -> (a -> m ()) -> m ()
whenLeft x f =
  case x of
    Left y -> f y
    Right _ -> pure ()

whenM :: Monad m => m Bool -> m () -> m ()
whenM x y =
  x >>= \case
    False -> pure ()
    True -> y
