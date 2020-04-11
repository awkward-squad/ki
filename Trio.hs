{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Trio where

import Control.Concurrent.Classy
import Data.Constraint
import Control.Exception
  ( AsyncException(ThreadKilled), Exception(..), SomeException, asyncExceptionFromException
  , asyncExceptionToException
  )
import Data.Foldable
import Data.Function
import Data.Typeable

data NurseryState m
  = Open [ Child m ]
  | Closed

data NurseryClosed
  = NurseryClosed
  deriving ( Show, Exception )

type Child m
  = ( ThreadId m, MVar m () )

type Nursery m
  = MVar m ( NurseryState m )

withNursery :: MonadConc m => ( Nursery m -> m a ) -> m a
withNursery k = do
  nursery <- newNursery

  uninterruptibleMask \unmask -> do
    result <- try ( unmask ( k nursery ) )
    cleanupNursery unmask nursery result

newNursery :: MonadConc m => m ( Nursery m )
newNursery =
  newMVar ( Open [] )

cleanupNursery
  :: MonadConc m
  => ( forall x. m x -> m x )
  -> Nursery m
  -> Either SomeException a
  -> m a
cleanupNursery unmask nursery result = do
  children <- closeNursery nursery

  case result of
    -- Running the callback resulted in an exception. It was either delivered to
    -- this thread asynchronously, was raised synchronously in the callback.
    -- Either way, we want to kill the children, then re-throw this exception.
    Left ex -> killChildrenThenThrow unmask children ex

    -- The callback completed. Now, we just wait for all of the children to
    -- finish.
    Right x -> do
      waitForChildren unmask ( reverse children )
      pure x

-- Preconditions:
--   * Asynchronous exceptions are uninterruptibly masked
--   * The nursery is open
closeNursery :: MonadConc m => Nursery m -> m [ Child m ]
closeNursery nursery =
  takeMVar nursery >>= \case
    Open children -> do
      putMVar nursery Closed
      pure children

-- Wait for then children to finish. If we receive an asynchronous exception
-- during this time, kill the children and re-throw the exception.
waitForChildren
  :: MonadConc m
  => ( forall x. m x -> m x )
  -> [ Child m ]
  -> m ()
waitForChildren unmask =
  fix \loop children0 ->
    case children0 of
      [] -> pure ()
      child : children ->
        try ( unmask ( waitForChild child ) ) >>= \case
          Left ex -> killChildrenThenThrow unmask ( child : children ) ex
          Right () -> loop children

waitForChild :: MonadConc m => Child m -> m ()
waitForChild ( _, doneVar ) =
  takeMVar doneVar

killChild :: MonadConc m => Child m -> m ()
killChild ( tid, doneVar ) = do
  killThread tid
  takeMVar doneVar

-- Kill the children, then re-throw the given exception.
killChildrenThenThrow
  :: MonadConc m
  => ( forall x. m x -> m x )
  -> [ Child m ]
  -> SomeException
  -> m a
killChildrenThenThrow unmask children ex = do
  for_ children \child ->
    -- Kill the child with asynchronous exceptions unmasked, because we don't
    -- want to deadlock with a child concurrently trying to throw an
    -- 'AsyncChildDied' back to us. But if any exceptions are thrown to us
    -- during this time, whether they are 'AsyncChildDied' or not, just ignore
    -- them. We already have an exception to throw, and we prefer it because
    -- it was delivered first.
    untilUninterrupted ( unmask ( killChild child ) )
  throw ( translateAsyncChildDied ex )

data ChildDied m
  = ChildDied
  { threadId :: ThreadId m
  , exception :: SomeException
  }

deriving instance Show ( ThreadId m ) => Show ( ChildDied m )
deriving anyclass instance ( Show ( ThreadId m ), Typeable m ) => Exception ( ChildDied m )

syncChildDied :: AsyncChildDied m -> ChildDied m
syncChildDied = \case
  AsyncChildDied tid ex -> ChildDied tid ex

data SomeAsyncChildDied
  = forall m.
    SomeAsyncChildDied
      ( Dict ( Show ( ThreadId m ), Typeable m ) )
      ( AsyncChildDied m )

instance Exception SomeAsyncChildDied where
  fromException = asyncExceptionFromException
  toException = asyncExceptionToException

instance Show SomeAsyncChildDied where
  show ( SomeAsyncChildDied Dict ex ) =
    show ex

data AsyncChildDied m
  = AsyncChildDied ( ThreadId m ) SomeException

deriving instance Show ( ThreadId m ) => Show ( AsyncChildDied m )

translateAsyncChildDied :: SomeException -> SomeException
translateAsyncChildDied ex =
  case fromException ex of
    Just ( SomeAsyncChildDied Dict ex' ) -> toException ( syncChildDied ex' )
    _ -> ex

spawn :: ( MonadConc m, Typeable m ) => Nursery m -> m () -> m ( ThreadId m )
spawn nursery action =
  mask_ do
    takeMVar nursery >>= \case
      Open children -> do
        throwToParent <- makeThrowToParent
        doneVar <- newEmptyMVar
        childThreadId <-
          forkWithUnmask \unmask -> do
            try ( unmask action ) >>= \case
              Poo ex -> throwToParent ex
              _ -> pure ()
            putMVar doneVar ()
        putMVar nursery ( Open ( ( childThreadId, doneVar ) : children ) )
        pure childThreadId

      Closed -> do
        putMVar nursery Closed
        throw NurseryClosed

try :: ( Exception e, MonadConc m ) => m a -> m ( Either e a )
try action = fmap Right action `catch` \ex -> pure ( Left ex )

-- Precondition: uninterruptibly masked inside callback
makeThrowToParent
  :: forall m.
     ( MonadConc m, Show ( ThreadId m ), Typeable m )
  => m ( SomeException -> m () )
makeThrowToParent = do
  parentThreadId <- myThreadId
  pure \ex -> do
    childThreadId <- myThreadId
    untilUninterrupted
      ( throwTo
          parentThreadId
          ( SomeAsyncChildDied Dict ( AsyncChildDied @m childThreadId ex ) )
      )

-- | Execute an IO action until it successfully completes, ignoring all
-- synchronous and asynchronous exceptions.
untilUninterrupted :: MonadConc m => m () -> m ()
untilUninterrupted action =
  fix \again ->
    catch @_ @SomeException action \_ -> again

pattern Poo :: SomeException -> Either SomeException a
pattern Poo ex <- Left ( poo -> Just ex )

poo :: SomeException -> Maybe SomeException
poo ex =
  case fromException ex of
    Just ThreadKilled -> Nothing
    _ -> Just ex
