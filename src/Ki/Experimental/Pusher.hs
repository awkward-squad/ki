module Ki.Experimental.Pusher
  ( pusher,
  )
where

import Control.Concurrent.STM
import Control.Monad
import qualified Ki.Fork
import Ki.Prelude
import Ki.Scope (Scope)
import qualified Ki.Scope

data S a
  = S !a !(TVar Bool)

pusher :: Scope -> ((a -> IO ()) -> IO ()) -> IO (STM (Maybe a))
pusher scope action = do
  closedVar <- newTVarIO False
  queue <- newTQueueIO

  let close :: IO ()
      close =
        atomically (writeTVar closedVar True)

  let produce :: IO ()
      produce =
        action \value -> do
          tookVar <- newTVarIO False
          atomicallyIO do
            writeTQueue queue $! S value tookVar
            pure (atomicallyIO (pure <$> (readTVar tookVar >>= check) <|> Ki.Scope.cancelledSTM scope))

  uninterruptibleMask \_ -> do
    Ki.Fork.forkWithUnmask scope \unmask -> do
      unmask produce `onException` close
      close

  let pull = do
        S value tookVar <- readTQueue queue
        writeTVar tookVar True
        pure (Just value)

  let closed :: STM (Maybe a)
      closed = do
        readTVar closedVar >>= check
        pure Nothing

  pure (pull <|> closed)

{-
_testProducer :: IO ()
_testProducer = do
  lock <- newMVar ()
  global do
    scoped \scope1 -> do
      recv <- producer scope1 go
      let r = atomically recv >>= withMVar lock . const . print
      scoped \scope2 -> do
        fork scope2 (replicateM_ 4 r)
        fork scope2 (replicateM_ 5 r)
        fork scope2 (replicateM_ 6 r)
        wait scope2
      cancel scope1
      wait scope1
  where
    go :: Context => (Int -> IO ()) -> IO ()
    go send = do
      scoped \scope -> do
        fork scope (loop 1)
        fork scope (loop 10)
        fork scope (loop 100)
        wait scope
      where
        loop :: Int -> IO ()
        loop n = do
          send n
          sleep 0.2
          loop (n + 1)
-}
