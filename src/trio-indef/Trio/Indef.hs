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
    scopeIsClosing,
    async,
    asyncMasked,
    await,
    cancel,
    Scope,
    Promise,
    RestoreMaskingState,
    ScopeClosed (..),
    ThreadGaveUp (..),
    ThreadFailed (..),
  )
where

import Control.Applicative ((<|>))
import Control.Exception (AsyncException (ThreadKilled), Exception (fromException, toException), SomeException, asyncExceptionFromException, asyncExceptionToException)
import Control.Monad (join, void, when)
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.Set (Set)
import qualified Data.Set as Set
import Trio.Internal.Conc (blockUntilTVar, registerBlock, retryingUntilSuccess)
import Trio.Sig (IO, STM, TMVar, TVar, ThreadId, atomically, catch, forkIOWithUnmask, modifyTVar', myThreadId, newEmptyTMVar, newTVarIO, putTMVar, readTMVar, readTVar, retry, throwIO, throwSTM, throwTo, try, uninterruptibleMask, uninterruptibleMask_, unsafeUnmask, writeTVar)
import Prelude hiding (IO)

-- import Trio.Internal.Debug

-- | A thread scope, which scopes the lifetime of threads spawned within it.
newtype Scope
  = Scope (TVar S)

data S = S
  { stateVar :: TVar State,
    runningVar :: TVar (Set ThreadId)
  }

data State
  = -- | The scope is open.
    Open Int -- Number of threads starting
  | -- | The scope has been marked as soft-closed, which is a suggestion to
    -- threads spawned within to finish. No more threads may be spawned within.
    SoftClosed Int -- Number of threads starting
  | -- | The scope is closed. No more threads may be spawned within.
    Closed

data Promise a = Promise
  { threadId :: ThreadId,
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

data ThreadFailed = ThreadFailed
  { threadId :: ThreadId,
    exception :: SomeException
  }
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Unexported async variant of 'ThreadFailed'.
data AsyncThreadFailed = AsyncThreadFailed
  { threadId :: ThreadId,
    exception :: SomeException
  }
  deriving stock (Show)

instance Exception AsyncThreadFailed where
  fromException = asyncExceptionFromException
  toException = asyncExceptionToException

translateAsyncThreadFailed :: SomeException -> SomeException
translateAsyncThreadFailed ex =
  case fromException ex of
    Just AsyncThreadFailed {threadId, exception} ->
      toException ThreadFailed {threadId, exception}
    _ -> ex

type RestoreMaskingState =
  forall x. IO x -> IO x

newScope :: IO (TVar S)
newScope = do
  stateVar <- newTVarIO "state" (Open 0)
  runningVar <- newTVarIO "running" Set.empty
  newTVarIO "scope" S {stateVar, runningVar}

withScope :: (Scope -> IO a) -> IO a
withScope f = do
  scopeVar <- newScope

  let action = do
        result <- f (Scope scopeVar)
        atomically (softJoinScope (Scope scopeVar))
        pure result

  uninterruptibleMask \restore ->
    restore action `catch` \exception -> do
      hardCloseScope (Scope scopeVar)
      throwIO (translateAsyncThreadFailed exception)

joinScope :: Scope -> Int -> IO ()
joinScope scope micros
  | micros < 0 = atomically (softJoinScope scope)
  | micros == 0 = uninterruptibleMask_ (hardCloseScope scope)
  | otherwise = do
    softClosed <- atomically (softCloseScope scope)
    when softClosed do
      blockUntilTimeout <- registerBlock micros
      let happyTeardown :: STM (IO ())
          happyTeardown =
            softJoinScope scope $> pure ()
      let sadTeardown :: STM (IO ())
          sadTeardown =
            blockUntilTimeout
              $> uninterruptibleMask_ (hardCloseScope scope)
      (join . atomically) (happyTeardown <|> sadTeardown)

-- | Block until all threads spawned within the scope finish, then close the
-- scope.
softJoinScope :: Scope -> STM ()
softJoinScope (Scope scopeVar) = do
  S {stateVar, runningVar} <- readTVar scopeVar
  let wait :: Int -> STM ()
      wait starting =
        if starting == 0
          then do
            blockUntilTVar runningVar Set.null
            writeTVar stateVar Closed
          else retry
  readTVar stateVar >>= \case
    Open starting -> wait starting
    SoftClosed starting -> wait starting
    Closed -> blockUntilTVar runningVar Set.null

-- | Soft-cancel a scope, and return whether it was left in the soft-closed
-- state (i.e. it wasn't already closed).
softCloseScope :: Scope -> STM Bool
softCloseScope (Scope scopeVar) = do
  S {stateVar} <- readTVar scopeVar
  readTVar stateVar >>= \case
    Open starting -> do
      writeTVar stateVar (SoftClosed starting)
      pure True
    SoftClosed _ -> pure True
    Closed -> pure False

-- Precondition: uninterruptibly masked
hardCloseScope :: Scope -> IO ()
hardCloseScope scope =
  atomically (setScopeToClosed scope) >>= \case
    Nothing -> pure ()
    Just childrenVar -> do
      cancellingThreadId <- myThreadId
      children <- atomically (readTVar childrenVar)
      for_ (Set.delete cancellingThreadId children) \child ->
        -- Kill the child with asynchronous exceptions unmasked, because
        -- we don't want to deadlock with a child concurrently trying to
        -- throw a exception back to us. But if any exceptions are thrown
        -- to us during this time, just ignore them. We already have an
        -- exception to throw, and we prefer it because it was delivered
        -- first.
        retryingUntilSuccess (unsafeUnmask (throwTo child ThreadKilled))

      if Set.member cancellingThreadId children
        then do
          atomically (blockUntilTVar childrenVar ((== 1) . Set.size))
          throwIO ThreadKilled
        else atomically (blockUntilTVar childrenVar Set.null)

-- | Wait for all threads to finish starting, then close a scope. Returns the
-- threads that are still running, if a state transition occurred (i.e. the
-- scope was not already closed).
setScopeToClosed :: Scope -> STM (Maybe (TVar (Set ThreadId)))
setScopeToClosed (Scope scopeVar) = do
  S {stateVar, runningVar} <- readTVar scopeVar
  let close :: Int -> STM (Maybe (TVar (Set ThreadId)))
      close starting = do
        when (starting /= 0) retry
        writeTVar stateVar Closed
        pure (Just runningVar)
  readTVar stateVar >>= \case
    Open starting -> close starting
    SoftClosed starting -> close starting
    Closed -> pure Nothing

scopeIsClosing :: Scope -> STM Bool
scopeIsClosing (Scope scopeVar) = do
  S {stateVar} <- readTVar scopeVar
  readTVar stateVar >>= \case
    Open _ -> pure False
    SoftClosed _ -> pure True
    Closed -> throwSTM ThreadKilled

-- | Spawn a thread within a scope.
async :: Scope -> IO a -> IO (Promise a)
async scope action =
  asyncMasked scope \unmask -> unmask action

-- | Like 'async', but spawns a thread with asynchronous exceptions
-- uninterruptibly masked, and provides the action with a function to unmask
-- asynchronous exceptions.
asyncMasked :: Scope -> (RestoreMaskingState -> IO a) -> IO (Promise a)
asyncMasked (Scope scopeVar) action = do
  uninterruptibleMask_ do
    S {stateVar, runningVar} <-
      atomically do
        scope@S {stateVar} <- readTVar scopeVar
        readTVar stateVar >>= \case
          Open starting -> do
            writeTVar stateVar $! (Open (starting + 1))
            pure scope
          SoftClosed starting -> do
            writeTVar stateVar $! (SoftClosed (starting + 1))
            pure scope
          Closed -> throwSTM ScopeClosed

    parentThreadId <- myThreadId
    resultVar <- atomically (newEmptyTMVar "result")

    childThreadId <-
      forkIOWithUnmask \unmask -> do
        childThreadId <- myThreadId
        result <- try (action unmask)
        case result of
          Left (NotThreadGaveUpOrKilled exception) ->
            throwTo parentThreadId (AsyncThreadFailed childThreadId exception)
          _ -> pure ()
        atomically do
          running <- readTVar runningVar
          if Set.member childThreadId running
            then do
              putTMVar resultVar result
              writeTVar runningVar $! Set.delete childThreadId running
            else retry

    atomically do
      modifyTVar' stateVar \case
        Open starting -> Open (starting - 1)
        _ -> error "scope not open"
      modifyTVar' runningVar (Set.insert childThreadId)

    pure
      Promise
        { threadId = childThreadId,
          resultVar
        }

-- | Wait for a promise to be fulfilled. If the thread failed, re-throws the
-- exception wrapped in a 'ThreadFailed'
await :: Promise a -> STM a
await Promise {resultVar, threadId} =
  readTMVar resultVar >>= \case
    Left exception -> throwSTM ThreadFailed {threadId, exception}
    Right result -> pure result

-- | Throw a 'ThreadKilled' to a thread, and wait for it to finish.
cancel :: Promise a -> IO ()
cancel Promise {resultVar, threadId} = do
  throwTo threadId ThreadKilled
  void (atomically (readTMVar resultVar))

pattern NotThreadGaveUpOrKilled :: SomeException -> SomeException
pattern NotThreadGaveUpOrKilled ex <-
  (asNotThreadGaveUpOrKilled -> Just ex)

asNotThreadGaveUpOrKilled :: SomeException -> Maybe SomeException
asNotThreadGaveUpOrKilled ex
  | Just ThreadKilled <- fromException ex = Nothing
  | Just ThreadGaveUp <- fromException ex = Nothing
  | otherwise = Just ex
