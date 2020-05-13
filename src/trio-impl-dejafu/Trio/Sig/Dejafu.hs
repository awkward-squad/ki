{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Trio.Sig.Dejafu where

import qualified Control.Concurrent.Classy as Conc
import qualified Control.Concurrent.Classy.Async as Async
import Control.Exception (Exception)
import qualified Test.DejaFu as Dejafu
import qualified Test.DejaFu.Conc.Internal.STM as Dejafu
import qualified Test.DejaFu.Types as Dejafu
import qualified Prelude
import Prelude hiding (IO)

type IO = Dejafu.Program Dejafu.Basic Prelude.IO

type STM = Dejafu.ModelSTM Prelude.IO

type ThreadId = Dejafu.ThreadId

type TMVar = Conc.TMVar STM

type TVar = Dejafu.ModelTVar Prelude.IO

atomically :: STM a -> IO a
atomically =
  Conc.atomically

catch :: forall e a. Exception e => IO a -> (e -> IO a) -> IO a
catch =
  Conc.catch

forkIOWithUnmask :: ((forall x. IO x -> IO x) -> IO ()) -> IO ThreadId
forkIOWithUnmask =
  Conc.forkWithUnmask

modifyTVar' :: TVar a -> (a -> a) -> STM ()
modifyTVar' =
  Conc.modifyTVar'

myThreadId :: IO ThreadId
myThreadId =
  Conc.myThreadId

newEmptyTMVar :: String -> STM (TMVar a)
newEmptyTMVar =
  Conc.newEmptyTMVarN

newTVarIO :: String -> a -> IO (TVar a)
newTVarIO name =
  Conc.atomically . Conc.newTVarN name

putTMVar :: TMVar a -> a -> STM ()
putTMVar =
  Conc.putTMVar

readTMVar :: TMVar a -> STM a
readTMVar =
  Conc.readTMVar

readTVar :: TVar a -> STM a
readTVar =
  Conc.readTVar

registerDelay :: Int -> IO (TVar Bool)
registerDelay =
  Conc.registerDelay

retry :: STM a
retry =
  Conc.retry

throwIO :: Exception e => e -> IO a
throwIO =
  Conc.throw

throwSTM :: Exception e => e -> STM a
throwSTM =
  Conc.throwSTM

throwTo :: Exception e => ThreadId -> e -> IO ()
throwTo =
  Conc.throwTo

try :: forall e a. Exception e => IO a -> IO (Either e a)
try action =
  Conc.catch (Right <$> action) (pure . Left)

uninterruptibleMask :: ((forall x. IO x -> IO x) -> IO a) -> IO a
uninterruptibleMask =
  Conc.uninterruptibleMask

uninterruptibleMask_ :: IO a -> IO a
uninterruptibleMask_ =
  Conc.uninterruptibleMask_

-- Not Good! Waiting for https://github.com/barrucadu/dejafu/issues/316
unsafeUnmask :: IO a -> IO a
unsafeUnmask action = do
  Async.withAsyncWithUnmask
    (\unmask -> unmask action)
    Async.wait

writeTVar :: TVar a -> a -> STM ()
writeTVar =
  Conc.writeTVar
