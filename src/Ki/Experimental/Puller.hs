{-# LANGUAGE TypeApplications #-}

module Ki.Experimental.Puller
  ( puller,
  )
where

import qualified Ki.Fork
import Ki.Prelude
import Ki.Scope (Scope)
import qualified Ki.Scope

puller :: Scope -> (IO a -> IO ()) -> IO (a -> STM ())
puller scope action = do
  queue <- newQ
  Ki.Fork.fork scope (action (pullQ queue (Ki.Scope.cancelledSTM scope)))
  pure (pushQ queue)

newtype Q a
  = Q (TQueue (TMVar a))

newQ :: forall a. IO (Q a)
newQ =
  coerce @(IO (TQueue (TMVar a))) newTQueueIO

pullQ :: Q a -> STM (IO a) -> IO a
pullQ (Q queue) fallback = do
  mvar <- newEmptyTMVarIO
  atomicallyIO do
    writeTQueue queue mvar
    pure (atomicallyIO (pure <$> readTMVar mvar <|> fallback))

pushQ :: Q a -> a -> STM ()
pushQ (Q queue) value = do
  mvar <- readTQueue queue
  putTMVar mvar value
