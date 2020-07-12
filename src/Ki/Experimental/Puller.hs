module Ki.Experimental.Puller
  ( puller,
  )
where

import Control.Monad
import qualified Ki.Fork
import Ki.Prelude
import Ki.Scope (Scope)
import qualified Ki.Scope

data S a
  = Busy
  | Waiting !(TMVar a)

puller :: Scope -> (IO a -> IO ()) -> IO (a -> STM ())
puller scope action = do
  var <- newTVarIO Busy

  Ki.Fork.fork scope do
    action do
      atomicallyIO do
        readTVar var >>= \case
          Busy -> do
            mvar <- newEmptyTMVar
            writeTVar var (Waiting mvar)
            pure (atomicallyIO (pure <$> readTMVar mvar <|> Ki.Scope.cancelledSTM scope))
          Waiting _ -> Ki.Scope.cancelledSTM scope

  pure \value ->
    readTVar var >>= \case
      Busy -> retry
      Waiting mvar -> do
        writeTVar var Busy
        putTMVar mvar value
