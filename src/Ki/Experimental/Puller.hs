module Ki.Experimental.Puller
  ( puller,
  )
where

import Control.Concurrent.STM
import qualified Ki.Fork
import Ki.Prelude
import Ki.Scope (Scope)
import qualified Ki.Scope

puller :: Scope -> (IO a -> IO ()) -> IO (a -> STM ())
puller scope action = do
  queue <- newTQueueIO

  Ki.Fork.fork scope do
    action do
      mvar <- newEmptyTMVarIO
      atomicallyIO do
        writeTQueue queue mvar
        pure (atomicallyIO (pure <$> readTMVar mvar <|> Ki.Scope.cancelledSTM scope))

  pure \value -> do
    mvar <- readTQueue queue
    putTMVar mvar value
