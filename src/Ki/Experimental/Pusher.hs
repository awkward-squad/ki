module Ki.Experimental.Pusher
  ( pusher,
  )
where

import Control.Monad
import Ki.Prelude
import Ki.Scope (Scope)
import qualified Ki.Scope
import Ki.Thread (Thread)
import qualified Ki.Thread

data S a
  = S !a !(TVar Bool)

pusher :: Scope -> ((a -> IO ()) -> IO ()) -> IO (STM (Maybe a))
pusher scope action = do
  queue <- newQ

  let push value = do
        tookVar <- newTVarIO False
        writeQ queue (S value tookVar)
        atomicallyIO (pure <$> (readTVar tookVar >>= check) <|> Ki.Scope.cancelledSTM scope)

  _ :: Thread () <-
    uninterruptibleMask \_ -> do
      Ki.Thread.forkWithUnmask scope \unmask -> do
        unmask (action push) `onException` closeQ queue
        closeQ queue

  let pull = do
        S value tookVar <- readQ queue
        writeTVar tookVar True
        pure (Just value)

  pure (pull <|> closedQ queue Nothing)

data Q a
  = Q !(TQueue a) !(TVar Bool)

newQ :: IO (Q a)
newQ =
  Q <$> newTQueueIO <*> newTVarIO False

closeQ :: Q a -> IO ()
closeQ (Q _ closedVar) =
  atomically (writeTVar closedVar True)

closedQ :: Q a -> b -> STM b
closedQ (Q _ closedVar) value = do
  readTVar closedVar >>= \case
    False -> retry
    True -> pure value

readQ :: Q a -> STM a
readQ (Q queue _) =
  readTQueue queue

writeQ :: Q a -> a -> IO ()
writeQ (Q queue _) value =
  atomically (writeTQueue queue $! value)
