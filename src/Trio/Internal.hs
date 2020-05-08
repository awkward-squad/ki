{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
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

import Control.Exception (Exception (..), SomeException, asyncExceptionFromException, asyncExceptionToException)
import Control.Monad.Conc.Class
import Control.Monad.IO.Class
import Data.Constraint
import Data.Foldable
import Data.Function
import Data.Functor (($>))
import Data.Typeable
import Trio.Internal.Conc
import Trio.Internal.Debug
import qualified Trio.Internal.Semaphore as Sem

type Nursery m =
  MVar m (NurseryState m)

data NurseryState m
  = Open [Child m]
  | Closed

data NurseryClosed
  = NurseryClosed
  deriving stock (Show)
  deriving anyclass (Exception)

withNursery :: (MonadConc m, MonadIO m) => (Nursery m -> m a) -> m a
withNursery f = do
  nursery <- newMVar (Open [])
  uninterruptibleMask \unmask -> do
    result <- try (unmask (f nursery))
    debug
      ( "Nursery callback "
          ++ case result of
            Left ex -> "exception: " ++ show ex
            Right _ -> "ended cleanly"
      )
    debug "Closing nursery"
    children <- closeNursery nursery
    debug "Closed nursery"
    case result of
      Left ex -> killChildrenThenThrow unmask children ex
      Right value -> waitForChildren unmask (reverse children) $> value

-- Preconditions:
--   * Asynchronous exceptions are uninterruptibly masked
--   * The nursery is open
closeNursery :: MonadConc m => Nursery m -> m [Child m]
closeNursery nursery =
  takeMVar nursery >>= \case
    Open children -> do
      putMVar nursery Closed
      pure children

-- Wait for then children to finish. If we receive an asynchronous exception
-- during this time, kill the children and re-throw the exception.
waitForChildren ::
  forall m.
  (MonadConc m, MonadIO m) =>
  (forall x. m x -> m x) ->
  [Child m] ->
  m ()
waitForChildren unmask =
  fix \loop children0 ->
    case children0 of
      [] -> pure ()
      child : children ->
        try (wait child) >>= \case
          Left ex -> killChildrenThenThrow unmask (child : children) ex
          Right () -> loop children
  where
    wait :: Child m -> m ()
    wait child = do
      debug ("Waiting for " ++ showChild @m child)
      catch @_ @SomeException
        (unmask (waitForChild child))
        \ex -> do
          debug ("Interrupted by '" ++ show ex ++ "'")
          throw ex

-- Kill the children, then re-throw the given exception. If the exception was
-- an 'AsyncChildDied', then translate it to a synchronous 'ChildDied' before
-- throwing.
killChildrenThenThrow ::
  forall a m.
  (MonadConc m, MonadIO m, Show (ThreadId m)) =>
  (forall x. m x -> m x) ->
  [Child m] ->
  SomeException ->
  m a
killChildrenThenThrow unmask children ex = do
  for_ children \child ->
    -- Kill the child with asynchronous exceptions unmasked, because we don't
    -- want to deadlock with a child concurrently trying to throw an
    -- 'AsyncChildDied' back to us. But if any exceptions are thrown to us
    -- during this time, whether they are 'AsyncChildDied' or not, just ignore
    -- them. We already have an exception to throw, and we prefer it because
    -- it was delivered first.
    untilUninterrupted do
      debug ("Unmasking to kill and wait for " ++ showChild @m child)
      catch @_ @SomeException
        ( unmask do
            debug ("Killing " ++ showChild @m child)
            killChild child
            debug ("Waiting for " ++ showChild @m child)
            waitForChild child
            debug ("Waited for " ++ showChild @m child)
        )
        ( \ex' -> do
            debug ("Interrupted by '" ++ show ex ++ "', ignoring")
            throw ex'
        )

  debug ("Throwing '" ++ show (translateAsyncChildDied ex) ++ "'")
  throw (translateAsyncChildDied ex)

forkMaskedChild ::
  (MonadConc m, MonadIO m, Typeable m) =>
  Nursery m ->
  ((forall x. m x -> m x) -> m ()) ->
  m (ThreadId m)
forkMaskedChild nursery action =
  addChild nursery do
    throwToParent <- makeThrowToParent
    sem <- Sem.new
    childThreadId <-
      forkMaskedChild_ action \result -> do
        for_ result throwToParent
        debug "Signaling done"
        Sem.signal sem
    pure (childThreadId, sem)

forkMaskedChild_ ::
  forall m.
  (MonadConc m, MonadIO m) =>
  ((forall x. m x -> m x) -> m ()) ->
  (Maybe SomeException -> m ()) ->
  m (ThreadId m)
forkMaskedChild_ action handler = do
  forkWithUnmask \unmask -> do
    debug "Running"
    try (run unmask) >>= \case
      Left (NotThreadKilled ex) -> handler (Just ex)
      _ -> handler Nothing
  where
    run :: (forall x. m x -> m x) -> m ()
    run unmask = do
      catch @_ @SomeException
        (action unmask)
        \ex -> do
          debug ("Exception: " ++ show ex)
          throw ex
      debug "Ended cleanly"

addChild ::
  forall m.
  (MonadConc m, MonadIO m) =>
  Nursery m ->
  m (Child m) ->
  m (ThreadId m)
addChild nursery action =
  mask_ do
    takeMVar nursery >>= \case
      Open children -> do
        child <- action
        debug ("Adding " ++ showChild @m child ++ " to nursery")
        putMVar nursery (Open (child : children))
        pure (fst child)
      Closed -> do
        putMVar nursery Closed
        throw NurseryClosed

-- Callback must be called with asynchronous exceptions masked
makeThrowToParent ::
  forall m.
  (MonadConc m, MonadIO m, Typeable m) =>
  m (SomeException -> m ())
makeThrowToParent = do
  parentThreadId <- myThreadId
  pure \ex -> do
    childThreadId <- myThreadId
    let ex' = AsyncChildDied @m Dict childThreadId ex
    debug ("Throwing '" ++ show ex' ++ "' to " ++ show parentThreadId)
    untilUninterrupted (throwTo parentThreadId ex')
    debug ("Threw '" ++ show ex' ++ "' to " ++ show parentThreadId)

--------------------------------------------------------------------------------
-- Child

type Child m =
  (ThreadId m, MVar m ())

waitForChild :: MonadConc m => Child m -> m ()
waitForChild (_, sem) =
  Sem.wait sem

killChild :: MonadConc m => Child m -> m ()
killChild (tid, _) = do
  killThread tid

showChild :: Show (ThreadId m) => Child m -> String
showChild (tid, _) =
  show tid

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
              ex' = ChildDied {..}
           in toException ex'
    _ -> ex
