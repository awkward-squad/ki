{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Trio.Internal
  ( withScope,
    asyncMasked,
    cancel,
    Scope,
    Async (..),
    ChildDied (..),
    ScopeClosed (..),
  )
where

import Control.Concurrent.Classy.STM
import Control.Exception (Exception (..), SomeException, asyncExceptionFromException, asyncExceptionToException)
import Control.Monad
import Control.Monad.Conc.Class
import Data.Constraint
import Data.Foldable
import qualified Data.Set as Set
import Data.Typeable
import Trio.Internal.Conc
-- import Trio.Internal.Debug
import Trio.Internal.Scope

data ScopeClosed
  = ScopeClosed
  deriving stock (Show)
  deriving anyclass (Exception)

type Unmask m =
  forall x. m x -> m x

withScope :: MonadConc m => (Scope m -> m a) -> m a
withScope f = do
  scopeVar <- newScope

  let action = do
        result <- f scopeVar
        softCloseScope scopeVar
        pure result

  uninterruptibleMask \unmask ->
    unmask action `catch` hardTeardown unmask scopeVar

-- | Tear down a scope, because we were hit with an asynchronous exception
-- either from the outside world, or from a spawned child telling us it failed.
--
-- First we close the scope, so no other children can spawn. Then, we kill all
-- children, and wait for them to finish. Finally, we re-throw the exception
-- that brought us down (but if it was an 'AsyncChildDied', then first translate
-- it to a synchronous 'ChildDied').
--
-- Preconditions:
--   * Asynchronous exceptions are uninterruptibly masked
hardTeardown ::
  MonadConc m =>
  Unmask m ->
  Scope m ->
  SomeException ->
  m a
hardTeardown unmask scopeVar exception = do
  childrenVar <- atomically (hardCloseScope scopeVar)

  children <- atomically (readTVar childrenVar)
  for_ children \child ->
    -- Kill the child with asynchronous exceptions unmasked, because we
    -- don't want to deadlock with a child concurrently trying to throw an
    -- 'AsyncChildDied' back to us. But if any exceptions are thrown to us
    -- during this time, whether they are 'AsyncChildDied' or not, just
    -- ignore them. We already have an exception to throw, and we prefer it
    -- because it was delivered first.
    retryingUntilSuccess (unmask (killThread child))

  atomically (blockUntilTVar childrenVar Set.null)

  throw (translateAsyncChildDied exception)

data Async m a = Async
  { threadId :: ThreadId m,
    action :: STM m (Either SomeException a)
  }

asyncMasked ::
  forall a m.
  (MonadConc m, Typeable m) =>
  Scope m ->
  (Unmask m -> m a) ->
  m (Async m a)
asyncMasked scopeVar action = do
  resultVar <- atomically newEmptyTMVar

  uninterruptibleMask_ do
    ScopeState {runningVar, startingVar} <-
      atomically do
        tryReadTMVar scopeVar >>= \case
          Nothing -> throwSTM ScopeClosed
          Just scope -> do
            modifyTVar' (startingVar scope) (+ 1)
            pure scope

    parentThreadId <- myThreadId

    childThreadId <-
      forkWithUnmask \unmask -> do
        childThreadId <- myThreadId
        result <- try (action unmask)
        case result of
          Left (NotThreadKilled exception) ->
            throwTo
              parentThreadId
              (AsyncChildDied @m Dict childThreadId exception)
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
        { threadId = childThreadId,
          action = readTMVar resultVar
        }

cancel :: MonadConc m => Async m a -> m ()
cancel Async {threadId, action} = do
  killThread threadId
  void (atomically action)

--------------------------------------------------------------------------------
-- ChildDied

data ChildDied m = ChildDied
  { threadId :: ThreadId m,
    exception :: SomeException
  }

deriving stock instance Show (ThreadId m) => Show (ChildDied m)

deriving anyclass instance
  (Show (ThreadId m), Typeable m) => Exception (ChildDied m)

data AsyncChildDied
  = forall m.
    AsyncChildDied
      (Dict (Show (ThreadId m), Typeable m))
      (ThreadId m)
      SomeException

instance Exception AsyncChildDied where
  fromException = asyncExceptionFromException
  toException = asyncExceptionToException

instance Show AsyncChildDied where
  show (AsyncChildDied Dict tid ex) =
    ("AsyncChildDied " ++ show tid ++ " " ++ show ex)

translateAsyncChildDied :: SomeException -> SomeException
translateAsyncChildDied ex =
  case fromException ex of
    Just (AsyncChildDied dict threadId exception) ->
      case dict of
        (Dict :: Dict (Show (ThreadId m), Typeable m)) ->
          let ex' :: ChildDied m
              ex' = ChildDied {threadId, exception}
           in toException ex'
    _ -> ex
