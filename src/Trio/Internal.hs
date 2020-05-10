{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Trio.Internal
  ( withScope,
    close,
    asyncMasked,
    await,
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
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable
import Trio.Internal.Conc (blockUntilTVar, retryingUntilSuccess, try, pattern NotThreadKilled)

-- import Trio.Internal.Debug

data Scope m = Scope
  { -- | Running children.
    runningVar :: TVar (STM m) (Set (ThreadId m)),
    -- | Number of children that are just about to start.
    startingVar :: TVar (STM m) Int
  }

data ScopeClosed
  = ScopeClosed
  deriving stock (Show)
  deriving anyclass (Exception)

type Unmask m =
  forall x. m x -> m x

newScope :: MonadConc m => m (TMVar (STM m) (Scope m))
newScope =
  atomically do
    runningVar <- newTVar Set.empty
    startingVar <- newTVar 0
    newTMVar Scope {runningVar, startingVar}

withScope :: MonadConc m => (TMVar (STM m) (Scope m) -> m a) -> m a
withScope f = do
  scopeVar <- newScope

  let action = do
        result <- f scopeVar
        scope <- atomically (readTMVar scopeVar)
        atomically do
          blockUntilTVar (runningVar scope) Set.null
          blockUntilTVar (startingVar scope) (== 0)
          void (takeTMVar scopeVar)
        pure result

  uninterruptibleMask \unmask ->
    unmask action `catch` \exception -> do
      closeWithUnmask unmask scopeVar
      throw (translateAsyncChildDied exception)

close :: MonadConc m => TMVar (STM m) (Scope m) -> m ()
close scopeVar =
  uninterruptibleMask \unmask ->
    closeWithUnmask unmask scopeVar

closeWithUnmask :: MonadConc m => Unmask m -> TMVar (STM m) (Scope m) -> m ()
closeWithUnmask unmask scopeVar =
  (join . atomically) do
    tryTakeTMVar scopeVar >>= \case
      Nothing -> pure (pure ())
      Just Scope {runningVar, startingVar} -> do
        starting <- readTVar startingVar
        unless (starting == 0) retry
        pure do
          children <- atomically (readTVar runningVar)
          for_ children \child ->
            -- Kill the child with asynchronous exceptions unmasked, because we
            -- don't want to deadlock with a child concurrently trying to throw
            -- an 'AsyncChildDied' back to us. But if any exceptions are thrown
            -- to us during this time, whether they are 'AsyncChildDied' or not,
            -- just ignore them. We already have an exception to throw, and we
            -- prefer it because it was delivered first.
            retryingUntilSuccess (unmask (killThread child))

          atomically (blockUntilTVar runningVar Set.null)

data Async m a = Async
  { threadId :: ThreadId m,
    action :: STM m (Either SomeException a)
  }

asyncMasked ::
  forall a m.
  (MonadConc m, Typeable m) =>
  TMVar (STM m) (Scope m) ->
  (Unmask m -> m a) ->
  m (Async m a)
asyncMasked scopeVar action = do
  resultVar <- atomically newEmptyTMVar

  uninterruptibleMask_ do
    Scope {runningVar, startingVar} <-
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

await :: forall a m. (MonadConc m, Typeable m) => Async m a -> STM m a
await Async {threadId, action} =
  action >>= \case
    Left exception -> throwSTM (ChildDied @m threadId exception)
    Right result -> pure result

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
