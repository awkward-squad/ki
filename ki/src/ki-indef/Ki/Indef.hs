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
    await,
    awaitFor,
    awaitSTM,
    cancel,
    cancelled,
    cancelledSTM,
    fork,
    forkWithUnmask,
    global,
    kill,
    scoped,
    timeout,
    wait,
    waitFor,
    waitSTM,
  )
where

import Ki.Indef.Context (Cancelled (..))
import qualified Ki.Indef.Context as Ki.Context
import Ki.Indef.Scope (Context, Scope, cancel, global, scoped)
import qualified Ki.Indef.Scope as Scope
import Ki.Indef.Seconds (Seconds)
import Ki.Indef.Thread (Thread, await, awaitFor, awaitSTM, kill, timeout)
import Ki.Sig (IO, STM, atomically, throwIO, unsafeUnmask)
import Prelude hiding (IO)

-- import Ki.Internal.Debug

async :: Scope -> (Context => IO a) -> IO (Thread a)
async scope action =
  Scope.async scope \restore -> restore action

asyncWithUnmask :: Scope -> (Context => (forall x. IO x -> IO x) -> IO a) -> IO (Thread a)
asyncWithUnmask scope action =
  Scope.async scope \restore -> restore (action unsafeUnmask)

cancelled :: Context => IO (Maybe (IO a))
cancelled =
  (fmap . fmap) (throwIO . Cancelled_) (Ki.Context.cancelled ?context)

cancelledSTM :: Context => STM (Maybe (IO a))
cancelledSTM =
  (fmap . fmap) (throwIO . Cancelled_) (Ki.Context.cancelledSTM ?context)

fork :: Scope -> (Context => IO ()) -> IO ()
fork scope action =
  Scope.fork scope \restore -> restore action

forkWithUnmask :: Scope -> (Context => (forall x. IO x -> IO x) -> IO ()) -> IO ()
forkWithUnmask scope action =
  Scope.fork scope \restore -> restore (action unsafeUnmask)

wait :: Scope -> IO ()
wait =
  atomically . Scope.wait

waitFor :: Scope -> Seconds -> IO ()
waitFor scope seconds =
  timeout seconds (pure <$> Scope.wait scope) (pure ())

waitSTM :: Scope -> STM ()
waitSTM =
  Scope.wait
