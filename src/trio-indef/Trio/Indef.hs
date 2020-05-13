{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Trio.Indef
  ( withScope,
    joinScope,
    softCancelScope,
    hardCancelScope,
    async,
    asyncMasked,
    await,
    softCancel,
    hardCancel,
    Scope,
    Promise,
    RestoreMaskingState,
    ChildDied (..),
    ScopeClosed (..),
    ThreadGaveUp (..),
  )
where

import Control.Exception (AsyncException (ThreadKilled), Exception (fromException, toException), SomeException, asyncExceptionFromException, asyncExceptionToException)
import Control.Monad (join, unless, void)
import Data.Foldable (for_)
import Data.Set (Set)
import qualified Data.Set as Set
import Trio.Internal.Conc (blockUntilTVar, retryingUntilSuccess)
import Trio.Sig (IO, STM, TMVar, TVar, ThreadId, atomically, catch, forkIOWithUnmask, modifyTVar', myThreadId, newEmptyTMVar, newTVarIO, putTMVar, readTMVar, readTVar, retry, throwIO, throwSTM, throwTo, try, uninterruptibleMask, uninterruptibleMask_, unsafeUnmask, writeTVar)
import Prelude hiding (IO)

-- import Trio.Internal.Debug

newtype Scope
  = Scope (TVar S)

data S = S
  { -- | Whether this scope is "cancelled", meaning children (who notice) should
    -- abort computation.
    cancelledVar :: TVar Bool,
    -- | Whether this scope is "closed" (disallowing new children).
    -- Invariant: if closed, then starting == 0, and running is either null or
    -- about to be (because we're currently killing all running threads).
    closedVar :: TVar Bool,
    -- | Running children.
    runningVar :: TVar (Set ThreadId),
    -- | Number of children that are just about to start.
    startingVar :: TVar Int
  }

data Promise a = Promise
  { threadId :: ThreadId,
    cancelledVar :: TVar Bool,
    resultVar :: TMVar (Either SomeException a)
  }

data ScopeClosed
  = ScopeClosed
  deriving stock (Show)
  deriving anyclass (Exception)

data ThreadGaveUp
  = ThreadGaveUp
  deriving stock (Show)
  deriving anyclass (Exception)

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

type RestoreMaskingState =
  forall x. IO x -> IO x

newScope :: IO (TVar S)
newScope = do
  cancelledVar <- newTVarIO "cancelled" False
  closedVar <- newTVarIO "closed" False
  runningVar <- newTVarIO "running" Set.empty
  startingVar <- newTVarIO "starting" 0
  newTVarIO "scope" S {cancelledVar, closedVar, runningVar, startingVar}

withScope :: (Scope -> IO a) -> IO a
withScope f = do
  scopeVar <- newScope

  let action = do
        result <- f (Scope scopeVar)
        atomically (joinScope (Scope scopeVar))
        pure result

  uninterruptibleMask \restore ->
    restore action `catch` \exception -> do
      hardCancelScopeWhileUninterruptiblyMasked scopeVar
      throwIO (translateAsyncChildDied exception)

joinScope :: Scope -> STM ()
joinScope (Scope scopeVar) = do
  S {closedVar, runningVar, startingVar} <- readTVar scopeVar
  closed <- readTVar closedVar
  if closed
    then blockUntilTVar runningVar Set.null
    else do
      blockUntilTVar startingVar (== 0)
      blockUntilTVar runningVar Set.null
      writeTVar closedVar True

softCancelScope :: Scope -> STM ()
softCancelScope (Scope scopeVar) = do
  S {closedVar, cancelledVar} <- readTVar scopeVar
  closed <- readTVar closedVar
  unless closed (writeTVar cancelledVar True)

hardCancelScope :: Scope -> IO ()
hardCancelScope (Scope scopeVar) =
  uninterruptibleMask_ (hardCancelScopeWhileUninterruptiblyMasked scopeVar)

hardCancelScopeWhileUninterruptiblyMasked :: TVar S -> IO ()
hardCancelScopeWhileUninterruptiblyMasked scopeVar =
  (join . atomically) do
    S {closedVar, runningVar, startingVar} <- readTVar scopeVar
    closed <- readTVar closedVar
    if closed
      then pure (pure ())
      else do
        blockUntilTVar startingVar (== 0)
        writeTVar closedVar True
        pure do
          cancellingThreadId <- myThreadId
          children <- atomically (readTVar runningVar)
          for_ (Set.delete cancellingThreadId children) \child ->
            -- Kill the child with asynchronous exceptions unmasked, because we
            -- don't want to deadlock with a child concurrently trying to throw
            -- a exception back to us. But if any exceptions are thrown to us
            -- during this time, just ignore them. We already have an exception
            -- to throw, and we prefer it because it was delivered first.
            retryingUntilSuccess (unsafeUnmask (throwTo child ThreadKilled))

          if Set.member cancellingThreadId children
            then do
              atomically (blockUntilTVar runningVar ((== 1) . Set.size))
              throwIO ThreadKilled
            else atomically (blockUntilTVar runningVar Set.null)

async :: Scope -> (STM void -> IO a) -> IO (Promise a)
async scope action =
  asyncMasked scope \unmask run -> unmask (action run)

asyncMasked ::
  Scope ->
  ( RestoreMaskingState ->
    STM void ->
    IO a
  ) ->
  IO (Promise a)
asyncMasked scope action = do
  uninterruptibleMask_ do
    S {cancelledVar = scopeCancelledVar, runningVar, startingVar} <-
      atomically do
        s@S {startingVar} <- assertScopeIsOpen scope
        modifyTVar' startingVar (+ 1)
        pure s

    parentThreadId <- myThreadId
    resultVar <- atomically (newEmptyTMVar "result")
    cancelledVar <- newTVarIO "cancelled" False

    childThreadId <-
      forkIOWithUnmask \unmask -> do
        childThreadId <- myThreadId
        result <-
          try do
            action
              unmask
              ( do
                  cancelled <-
                    readTVar scopeCancelledVar >>= \case
                      False -> readTVar cancelledVar
                      True -> pure True
                  if cancelled
                    then throwSTM ThreadGaveUp
                    else retry
              )
        case result of
          Left (NotThreadGaveUpOrKilled exception) ->
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
      Promise
        { threadId = childThreadId,
          cancelledVar,
          resultVar
        }

await :: Promise a -> STM a
await Promise {resultVar, threadId} =
  readTMVar resultVar >>= \case
    Left exception -> throwSTM ChildDied {threadId, exception}
    Right result -> pure result

softCancel :: Promise a -> STM ()
softCancel Promise {cancelledVar} =
  writeTVar cancelledVar True

hardCancel :: Promise a -> IO ()
hardCancel Promise {resultVar, threadId} = do
  throwTo threadId ThreadKilled
  void (atomically (readTMVar resultVar))

assertScopeIsOpen :: Scope -> STM S
assertScopeIsOpen (Scope scopeVar) = do
  scope@S {closedVar} <- readTVar scopeVar
  closed <- readTVar closedVar
  if closed
    then throwSTM ScopeClosed
    else pure scope

pattern NotThreadGaveUpOrKilled :: SomeException -> SomeException
pattern NotThreadGaveUpOrKilled ex <-
  (asNotThreadGaveUpOrKilled -> Just ex)

asNotThreadGaveUpOrKilled :: SomeException -> Maybe SomeException
asNotThreadGaveUpOrKilled ex
  | Just ThreadKilled <- fromException ex = Nothing
  | Just ThreadGaveUp <- fromException ex = Nothing
  | otherwise = Just ex
