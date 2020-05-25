{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Ki.Indef
  ( Cancelled (..),
    Context,
    Scope,
    Seconds,
    Thread,
    async,
    asyncWithUnmask,
    asyncWithUnmask_,
    async_,
    await,
    awaitFor,
    awaitSTM,
    background,
    cancel,
    cancelled,
    cancelledSTM,
    kill,
    scoped,
    timeout,
    wait,
    waitFor,
    waitSTM,
  )
where

import Data.Functor (void)
import Ki.Indef.Context (Cancelled (..), Context, background)
import qualified Ki.Indef.Context as Ki.Context
import Ki.Indef.Scope (Scope, cancel, scoped)
import qualified Ki.Indef.Scope as Scope
import Ki.Indef.Seconds (Seconds)
import Ki.Indef.Thread (Thread, await, awaitFor, awaitSTM, kill, timeout)
import Ki.Sig (IO, STM, atomically, throwIO, unsafeUnmask)
import Prelude hiding (IO)

-- import Ki.Internal.Debug

wait :: Scope -> IO ()
wait =
  atomically . Scope.wait

waitFor :: Scope -> Seconds -> IO ()
waitFor scope seconds =
  timeout seconds (pure <$> Scope.wait scope) (pure ())

waitSTM :: Scope -> STM ()
waitSTM =
  Scope.wait

cancelled :: Context -> IO (Maybe (IO a))
cancelled =
  (fmap . fmap) (throwIO . Cancelled_) . Ki.Context.cancelled

cancelledSTM :: Context -> STM (Maybe (IO a))
cancelledSTM =
  (fmap . fmap) (throwIO . Cancelled_) . Ki.Context.cancelledSTM

async :: Scope -> (Context -> IO a) -> IO (Thread a)
async scope action =
  Scope.async scope \context restore -> restore (action context)

async_ :: Scope -> (Context -> IO a) -> IO ()
async_ scope action =
  void (async scope action)

asyncWithUnmask ::
  Scope ->
  (Context -> (forall x. IO x -> IO x) -> IO a) ->
  IO (Thread a)
asyncWithUnmask scope action =
  Scope.async scope \context restore -> restore (action context unsafeUnmask)

asyncWithUnmask_ ::
  Scope ->
  (Context -> (forall x. IO x -> IO x) -> IO a) ->
  IO ()
asyncWithUnmask_ scope f =
  void (asyncWithUnmask scope f)
