{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Ki.Indef.Scope
  ( Scope,
    async,
    async_,
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
import qualified Data.Semigroup as Semigroup
import Data.Set (Set)
import qualified Data.Set as Set
import Ki.Indef.Context (CancelToken (..), Cancelled (..), Context)
import qualified Ki.Indef.Context as Ki.Context
import Ki.Indef.Thread (AsyncThreadFailed (..), Thread (Thread), timeout)
import qualified Ki.Indef.Thread as Thread
import Ki.Sig (IO, STM, TVar, ThreadId, atomically, forkIO, modifyTVar', myThreadId, newEmptyTMVarIO, newTVar, newUnique, putTMVar, readTVar, retry, throwIO, throwSTM, throwTo, try, uninterruptibleMask, unsafeUnmask, writeTVar)
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

async ::
  Scope ->
  (Context -> (forall x. IO x -> IO x) -> IO a) ->
  IO (Thread a)
async scope@Scope {context, runningVar} action = do
  uninterruptibleMask \restore -> do
    _asyncBeforeForking scope
    parentThreadId <- myThreadId
    resultVar <- newEmptyTMVarIO "result"
    childThreadId <-
      forkIO do
        result <- try (action context restore)
        _asyncWithResult context parentThreadId result
        childThreadId <- myThreadId
        _asyncAfterRunning scope childThreadId \running -> do
          putTMVar resultVar result
          writeTVar runningVar $! Set.delete childThreadId running
    _asyncAfterForking scope childThreadId
    pure (Thread childThreadId resultVar)

async_ ::
  Scope ->
  (Context -> (forall x. IO x -> IO x) -> IO a) ->
  IO ()
async_ scope@Scope {context, runningVar} action = do
  uninterruptibleMask \restore -> do
    _asyncBeforeForking scope
    parentThreadId <- myThreadId
    childThreadId <-
      forkIO do
        result <- try (action context restore)
        _asyncWithResult context parentThreadId result
        childThreadId <- myThreadId
        _asyncAfterRunning scope childThreadId \running ->
          writeTVar runningVar $! Set.delete childThreadId running
    _asyncAfterForking scope childThreadId

_asyncBeforeForking :: Scope -> IO ()
_asyncBeforeForking Scope{closedVar, startingVar} = do
  atomically do
    readTVar closedVar >>= \case
      False -> modifyTVar' startingVar (+ 1)
      True -> throwSTM (ErrorCall "ki: scope closed")

_asyncAfterForking :: Scope -> ThreadId -> IO ()
_asyncAfterForking Scope{startingVar, runningVar} childThreadId =
  atomically do
    modifyTVar' startingVar (subtract 1)
    modifyTVar' runningVar (Set.insert childThreadId)

_asyncWithResult :: Context -> ThreadId -> Either SomeException a -> IO ()
_asyncWithResult context parentThreadId result =
  whenLeft result \exception -> do
    whenM
      (shouldPropagateException context exception)
      (throwTo parentThreadId (AsyncThreadFailed exception))

_asyncAfterRunning :: Scope -> ThreadId -> (Set ThreadId -> STM ()) -> IO ()
_asyncAfterRunning Scope{runningVar} childThreadId action =
  atomically do
    running <- readTVar runningVar
    if Set.member childThreadId running
      then action running
      else retry

cancel :: Scope -> IO ()
cancel Scope {context} = do
  token <- newUnique
  atomically (Ki.Context.cancel context (CancelToken token))

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
        pure
          Scope
            { context,
              closedVar,
              runningVar,
              startingVar
            }

    -- Close a scope, kill all of the running threads, and return the first
    -- async exception delivered to us while doing so, if any.
    --
    -- Preconditions:
    --   * The set of threads doesn't include us
    --   * We're uninterruptibly masked
    closeScope :: Scope -> IO (Maybe SomeException)
    closeScope Scope {closedVar, runningVar, startingVar} = do
      threads <-
        atomically do
          blockUntilTVar startingVar (== 0)
          writeTVar closedVar True
          readTVar runningVar
      exception <- killThreads (Set.toList threads)
      atomically (blockUntilTVar runningVar Set.null)
      pure
        ( coerce
            @(Maybe (Semigroup.First SomeException))
            @(Maybe SomeException)
            exception
        )
      where
        killThreads :: [ThreadId] -> IO (Maybe (Semigroup.First SomeException))
        killThreads =
          loop Nothing
          where
            loop ::
              Maybe (Semigroup.First SomeException) ->
              [ThreadId] ->
              IO (Maybe (Semigroup.First SomeException))
            loop acc = \case
              [] -> pure acc
              threadId : threadIds ->
                -- We unmask because we don't want to deadlock with a thread
                -- that is concurrently trying to throw an exception to us with
                -- exceptions masked.
                try (unsafeUnmask (throwTo threadId ThreadKilled)) >>= \case
                  Left exception ->
                    loop
                      (acc <> Just (Semigroup.First exception))
                      (threadId : threadIds) -- don't drop thread we didn't kill
                  Right () -> loop acc threadIds

wait :: Scope -> STM ()
wait Scope {runningVar, startingVar} = do
  blockUntilTVar runningVar Set.null
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
        Just (Cancelled_ token) -> do
          token' <- Ki.Context.cancelled context
          pure (token' /= Just token)
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
