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
    ScopeClosed (..),
    ThreadGaveUp (..),
    ThreadFailed (..),
  )
where

import Control.Exception (AsyncException (ThreadKilled), Exception (fromException, toException), SomeException, asyncExceptionFromException, asyncExceptionToException)
import Control.Monad (join, void, when)
import Data.Foldable (for_)
import Data.Set (Set)
import qualified Data.Set as Set
import Trio.Internal.Conc (blockUntilTVar, retryingUntilSuccess)
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
  | -- | The scope has been marked as soft-cancelled, which is a suggestion to
    -- threads spawned within to finish. No more threads may be spawned within.
    Cancelled Int -- Number of threads starting
  | -- | The scope is closed. No more threads may be spawned within.
    Closed

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

-- | Perform an action with a new thread scope.
--
-- * If the action throws an exception, it first hard-cancels the scope (exactly
-- as if 'hardCancelScope' was called).
--
-- * If any thread spawned within the scope throws an exception other than
-- 'ThreadKilled' or 'ThreadGaveUp', it is propagated to this thread, which then
-- hard-cancels the scope (exactly as if 'hardCancelScope was called') and
-- re-throws the exception wrapped in a 'ThreadFailed'.
--
-- * If the action completes, blocks until all threads spawned within the scope
-- finish, then closes the scope (exactly as if 'joinScope' was called).
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
      throwIO (translateAsyncThreadFailed exception)

-- | Block until all threads spawned within the scope finish, then close the
-- scope.
joinScope :: Scope -> STM ()
joinScope (Scope scopeVar) = do
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
    Cancelled starting -> wait starting
    Closed ->
      blockUntilTVar runningVar Set.null

-- | Designate a scope as soft cancelled, which is a suggestion to threads
-- spawned within it to finish.
softCancelScope :: Scope -> STM ()
softCancelScope (Scope scopeVar) = do
  S {stateVar} <- readTVar scopeVar
  readTVar stateVar >>= \case
    Open starting -> writeTVar stateVar (Cancelled starting)
    Cancelled _ -> pure ()
    Closed -> pure ()

-- | Close a scope, throw a 'ThreadKilled' to each thread spawned within it, and
-- wait for them to finish.
hardCancelScope :: Scope -> IO ()
hardCancelScope (Scope scopeVar) =
  uninterruptibleMask_ (hardCancelScopeWhileUninterruptiblyMasked scopeVar)

hardCancelScopeWhileUninterruptiblyMasked :: TVar S -> IO ()
hardCancelScopeWhileUninterruptiblyMasked scopeVar =
  (join . atomically) do
    S {stateVar, runningVar} <- readTVar scopeVar
    let close :: Int -> STM (IO ())
        close starting = do
          when (starting /= 0) retry
          writeTVar stateVar Closed
          pure do
            cancellingThreadId <- myThreadId
            children <- atomically (readTVar runningVar)
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
                atomically (blockUntilTVar runningVar ((== 1) . Set.size))
                throwIO ThreadKilled
              else atomically (blockUntilTVar runningVar Set.null)

    readTVar stateVar >>= \case
      Open starting -> close starting
      Cancelled starting -> close starting
      Closed -> pure (pure ())

-- | Spawn a thread within a scope.
--
-- The action is provided an @STM void@ action, which never returns, but throws
-- a 'ThreadGaveUp' exception if this thread or the enclosing scope were
-- soft-cancelled.
--
-- @
-- async scope \\giveUp -> do
--   work
--   atomically (giveUp <|> pure ())
--   work
--   atomically (giveUp <|> pure ())
--   work
-- @
async :: Scope -> (STM void -> IO a) -> IO (Promise a)
async scope action =
  asyncMasked scope \unmask run -> unmask (action run)

-- | Like 'async', but spawns a thread with asynchronous exceptions
-- uninterruptibly masked, and provides the action with a function to unmask
-- asynchronous exceptions.
asyncMasked ::
  Scope ->
  ( RestoreMaskingState ->
    STM void ->
    IO a
  ) ->
  IO (Promise a)
asyncMasked (Scope scopeVar) action = do
  uninterruptibleMask_ do
    S {stateVar, runningVar} <-
      atomically do
        scope@S {stateVar} <- readTVar scopeVar
        readTVar stateVar >>= \case
          Open starting -> do
            writeTVar stateVar $! (Open (starting + 1))
            pure scope
          Cancelled _ -> throwSTM ScopeClosed
          Closed -> throwSTM ScopeClosed

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
                  readTVar stateVar >>= \case
                    Open _ -> retry
                    Cancelled _ -> throwSTM ThreadGaveUp
                    Closed -> throwSTM ThreadGaveUp
              )
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
          cancelledVar,
          resultVar
        }

-- | Wait for a promise to be fulfilled. If the thread failed, re-throws the
-- exception wrapped in a 'ThreadFailed'
await :: Promise a -> STM a
await Promise {resultVar, threadId} =
  readTMVar resultVar >>= \case
    Left exception -> throwSTM ThreadFailed {threadId, exception}
    Right result -> pure result

-- | Designate a thread as soft-cancelled, which is a suggestion for it to
-- finish.
softCancel :: Promise a -> STM ()
softCancel Promise {cancelledVar} =
  writeTVar cancelledVar True

-- | Throw a 'ThreadKilled' to a thread, and wait for it to finish.
hardCancel :: Promise a -> IO ()
hardCancel Promise {resultVar, threadId} = do
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
