{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}

module Trio
  ( withScope,
    joinScope,
    cancelScope,
    async,
    async_,
    asyncMasked,
    asyncMasked_,
    Scope,
    Async (..),
    ChildDied (..),
    ScopeClosed (..),
  )
where

import Control.Exception (AsyncException (ThreadKilled), Exception (fromException, toException), SomeException, asyncExceptionFromException, asyncExceptionToException)
import Control.Monad (join, unless, void)
import Data.Foldable (for_)
import Data.Set (Set)
import qualified Data.Set as Set
import Trio.Internal.Conc (blockUntilTVar, retryingUntilSuccess, pattern NotThreadKilled)
import Trio.Sig (IO, STM, TVar, ThreadId, atomically, catch, forkIOWithUnmask, modifyTVar', myThreadId, newEmptyTMVar, newTVarIO, putTMVar, readTMVar, readTVar, retry, throwIO, throwSTM, throwTo, try, uninterruptibleMask, uninterruptibleMask_, unsafeUnmask, writeTVar)
import Prelude hiding (IO)

-- import Trio.Internal.Debug

newtype Scope
  = Scope (TVar S)

data S = S
  { -- | Whether this scope is "closed" (disallowing new children).
    -- Invariant: if closed, then starting == 0, and running is either about to
    -- be 0 (because we are killing all children), about to be 1 (because a
    -- child is cancelling the scope, which kills all other children), or
    -- already 0 or 1 due of one of those circumstances.
    closed :: Bool,
    -- | Running children.
    runningVar :: TVar (Set ThreadId),
    -- | Number of children that are just about to start.
    startingVar :: TVar Int
  }

data ScopeClosed
  = ScopeClosed
  deriving stock (Show)
  deriving anyclass (Exception)

newScope :: IO (TVar S)
newScope = do
  runningVar <- newTVarIO "running" Set.empty
  startingVar <- newTVarIO "starting" 0
  newTVarIO "scope" S {closed = False, runningVar, startingVar}

withScope :: (Scope -> IO a) -> IO a
withScope f = do
  scopeVar <- newScope

  let action = do
        result <- f (Scope scopeVar)
        atomically (joinScope (Scope scopeVar))
        pure result

  uninterruptibleMask \restore ->
    restore action `catch` \exception -> do
      cancelScopeWhileUninterruptiblyMasked scopeVar
      throwIO (translateAsyncChildDied exception)

-- | Wait for all children to finish, and close the scope.
joinScope :: Scope -> STM ()
joinScope (Scope scopeVar) = do
  S {closed, runningVar, startingVar} <- readTVar scopeVar
  unless closed (blockUntilTVar startingVar (== 0))
  blockUntilTVar runningVar Set.null
  writeTVar scopeVar S {closed = True, runningVar, startingVar}

cancelScope :: Scope -> IO ()
cancelScope (Scope scopeVar) =
  uninterruptibleMask_ (cancelScopeWhileUninterruptiblyMasked scopeVar)

cancelScopeWhileUninterruptiblyMasked :: TVar S -> IO ()
cancelScopeWhileUninterruptiblyMasked scopeVar =
  (join . atomically) do
    S {closed, runningVar, startingVar} <- readTVar scopeVar
    if closed
      then pure (pure ())
      else do
        blockUntilTVar startingVar (== 0)
        writeTVar scopeVar S {closed = True, runningVar, startingVar}
        pure do
          cancellingThreadId <- myThreadId
          children <- atomically (readTVar runningVar)
          for_ (Set.delete cancellingThreadId children) \child ->
            -- Kill the child with asynchronous exceptions unmasked, because we
            -- don't want to deadlock with a child concurrently trying to throw
            -- an 'AsyncChildDied' back to us. But if any exceptions are thrown
            -- to us during this time, whether they are 'AsyncChildDied' or not,
            -- just ignore them. We already have an exception to throw, and we
            -- prefer it because it was delivered first.
            retryingUntilSuccess (unsafeUnmask (throwTo child ThreadKilled))

          if Set.member cancellingThreadId children
            then do
              atomically (blockUntilTVar runningVar ((== 1) . Set.size))
              throwTo cancellingThreadId ThreadKilled -- kill self
            else atomically (blockUntilTVar runningVar Set.null)

data Async a = Async
  { await :: STM a,
    cancel :: IO ()
  }

async :: Scope -> IO a -> IO (Async a)
async scope action =
  asyncMasked scope \unmask -> unmask action

async_ :: Scope -> IO a -> IO ()
async_ scope action =
  void (async scope action)

asyncMasked :: Scope -> ((forall x. IO x -> IO x) -> IO a) -> IO (Async a)
asyncMasked (Scope scopeVar) action = do
  resultVar <- atomically (newEmptyTMVar "result")

  uninterruptibleMask_ do
    S {runningVar, startingVar} <-
      atomically do
        scope@(S {closed, startingVar}) <- readTVar scopeVar
        if closed
          then throwSTM ScopeClosed
          else do
            modifyTVar' startingVar (+ 1)
            pure scope

    parentThreadId <- myThreadId

    childThreadId <-
      forkIOWithUnmask \unmask -> do
        childThreadId <- myThreadId
        result <- try (action unmask)
        case result of
          Left (NotThreadKilled exception) ->
            throwTo parentThreadId (AsyncChildDied childThreadId exception)
          _ -> pure ()
        atomically do
          running <- readTVar runningVar
          if Set.member childThreadId running
            then do
              putTMVar resultVar result
              writeTVar runningVar $! Set.delete childThreadId running
            else retry

    atomically do
      modifyTVar' startingVar (subtract 1)
      modifyTVar' runningVar (Set.insert childThreadId)

    pure
      Async
        { await = readTMVar resultVar >>= \case
            Left exception -> throwSTM (ChildDied childThreadId exception)
            Right result -> pure result,
          cancel = do
            throwTo childThreadId ThreadKilled
            void (atomically (readTMVar resultVar))
        }

asyncMasked_ :: Scope -> ((forall x. IO x -> IO x) -> IO a) -> IO ()
asyncMasked_ scope action =
  void (asyncMasked scope action)

--------------------------------------------------------------------------------
-- ChildDied

data ChildDied = ChildDied
  { threadId :: ThreadId,
    exception :: SomeException
  }
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Unexported async variant of 'ChildDied'.
data AsyncChildDied = AsyncChildDied
  { threadId :: ThreadId,
    exception :: SomeException
  }
  deriving stock (Show)

instance Exception AsyncChildDied where
  fromException = asyncExceptionFromException
  toException = asyncExceptionToException

translateAsyncChildDied :: SomeException -> SomeException
translateAsyncChildDied ex =
  case fromException ex of
    Just AsyncChildDied {threadId, exception} ->
      toException ChildDied {threadId, exception}
    _ -> ex
