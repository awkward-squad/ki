{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Trio.Internal
  ( withNursery,
    forkMaskedChild,
    Nursery,
    ChildDied (..),
    NurseryClosed (..),
  )
where

import Control.Concurrent.Classy.STM
import Control.Exception (Exception (..), SomeException, asyncExceptionFromException, asyncExceptionToException)
import Control.Monad.Conc.Class
import Data.Constraint
import Data.Foldable
import qualified Data.Set as Set
import Data.Typeable
import Trio.Internal.Conc
-- import Trio.Internal.Debug
import Trio.Internal.Nursery

data NurseryClosed
  = NurseryClosed
  deriving stock (Show)
  deriving anyclass (Exception)

type Unmask m =
  forall x. m x -> m x

withNursery :: MonadConc m => (Nursery m -> m a) -> m a
withNursery f = do
  nurseryVar <- newNursery

  let action = do
        result <- f nurseryVar
        softCloseNursery nurseryVar
        pure result

  uninterruptibleMask \unmask ->
    unmask action `catch` hardTeardown unmask nurseryVar

-- | Tear down a nursery, because we were hit with an asynchronous exception
-- either from the outside world, or from a spawned child telling us it failed.
--
-- First we close the nursery, so no other children can spawn. Then, we kill all
-- children, and wait for them to finish. Finally, we re-throw the exception
-- that brought us down (but if it was an 'AsyncChildDied', then first translate
-- it to a synchronous 'ChildDied').
--
-- Preconditions:
--   * Asynchronous exceptions are uninterruptibly masked
hardTeardown ::
  MonadConc m =>
  Unmask m ->
  Nursery m ->
  SomeException ->
  m a
hardTeardown unmask nurseryVar exception = do
  childrenVar <- atomically (hardCloseNursery nurseryVar)

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

forkMaskedChild ::
  forall m.
  (MonadConc m, Typeable m) =>
  Nursery m ->
  (Unmask m -> m ()) ->
  m (ThreadId m)
forkMaskedChild nurseryVar action =
  uninterruptibleMask_ do
    NurseryState {runningVar, startingVar} <-
      atomically do
        tryReadTMVar nurseryVar >>= \case
          Nothing -> throwSTM NurseryClosed
          Just nursery -> do
            modifyTVar' (startingVar nursery) (+ 1)
            pure nursery

    parentThreadId <- myThreadId

    childThreadId <-
      forkWithUnmask \unmask -> do
        childThreadId <- myThreadId
        try (action unmask) >>= \case
          Left (NotThreadKilled exception) -> do
            throwTo
              parentThreadId
              (AsyncChildDied @m Dict childThreadId exception)
          _ -> pure ()
        atomically do
          running <- readTVar runningVar
          if Set.member childThreadId running
            then writeTVar runningVar $! Set.delete childThreadId running
            else retry

    atomically do
      modifyTVar' startingVar (subtract 1)
      modifyTVar' runningVar (Set.insert childThreadId)

    pure childThreadId

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
