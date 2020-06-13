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
    background,
    cancel,
    cancelled,
    cancelledSTM,
    fork,
    forkWithUnmask,
    kill,
    scoped,
    timeout,
    wait,
    waitFor,
    waitSTM,
  )
where

import Ki.Indef.Context (Cancelled (..), Context, background)
import qualified Ki.Indef.Context as Ki.Context
import Ki.Indef.Scope (Scope, cancel, scoped)
import qualified Ki.Indef.Scope as Scope
import Ki.Indef.Seconds (Seconds)
import Ki.Indef.Thread (Thread, await, awaitFor, awaitSTM, kill, timeout)
import Ki.Sig (IO, STM, atomically, throwIO, unsafeUnmask)
import Prelude hiding (IO)

-- import Ki.Internal.Debug

async :: Scope -> (Context -> IO a) -> IO (Thread a)
async scope action =
  Scope.async scope \context restore -> restore (action context)

asyncWithUnmask ::
  Scope ->
  (Context -> (forall x. IO x -> IO x) -> IO a) ->
  IO (Thread a)
asyncWithUnmask scope action =
  Scope.async scope \context restore -> restore (action context unsafeUnmask)

cancelled :: Context -> IO (Maybe (IO a))
cancelled =
  (fmap . fmap) (throwIO . Cancelled_) . Ki.Context.cancelled

cancelledSTM :: Context -> STM (Maybe (IO a))
cancelledSTM =
  (fmap . fmap) (throwIO . Cancelled_) . Ki.Context.cancelledSTM

fork :: Scope -> (Context -> IO a) -> IO ()
fork scope action =
  Scope.fork scope \context restore -> restore (action context)

forkWithUnmask :: Scope -> (Context -> (forall x. IO x -> IO x) -> IO a) -> IO ()
forkWithUnmask scope action =
  Scope.fork scope \context restore -> restore (action context unsafeUnmask)

wait :: Scope -> IO ()
wait =
  atomically . Scope.wait

waitFor :: Scope -> Seconds -> IO ()
waitFor scope seconds =
  timeout seconds (pure <$> Scope.wait scope) (pure ())

waitSTM :: Scope -> STM ()
waitSTM =
  Scope.wait
