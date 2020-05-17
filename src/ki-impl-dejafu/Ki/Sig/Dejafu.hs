{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Ki.Sig.Dejafu (module Ki.Sig.Dejafu) where

import qualified Control.Concurrent.Classy as Conc
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

forkIO :: IO () -> IO ThreadId
forkIO =
  Conc.fork

modifyTVar' :: TVar a -> (a -> a) -> STM ()
modifyTVar' =
  Conc.modifyTVar'

myThreadId :: IO ThreadId
myThreadId =
  Conc.myThreadId

newEmptyTMVar :: String -> STM (TMVar a)
newEmptyTMVar =
  Conc.newEmptyTMVarN

newTVar :: String -> a -> STM (TVar a)
newTVar name =
  Conc.newTVarN name

putTMVar :: TMVar a -> a -> STM ()
putTMVar =
  Conc.putTMVar

readTMVar :: TMVar a -> STM a
readTMVar =
  Conc.readTMVar

readTVar :: TVar a -> STM a
readTVar =
  Conc.readTVar

-- registerDelay :: Int -> IO (TVar Bool)
-- registerDelay =
--   Conc.registerDelay

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

unsafeUnmask :: IO a -> IO a
unsafeUnmask = do
  Conc.unsafeUnmask

writeTVar :: TVar a -> a -> STM ()
writeTVar =
  Conc.writeTVar
