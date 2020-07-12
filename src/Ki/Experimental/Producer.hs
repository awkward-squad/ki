module Ki.Experimental.Producer
  ( producer,
  )
where

import Control.Monad
import Ki.Concurrency
import Ki.Implicit
import Ki.Prelude

data S a
  = Closed
  | Empty
  | Full !a !(TMVar ())

producer :: Scope -> (Context => (a -> IO ()) -> IO ()) -> IO (STM (Maybe a))
producer scope action = do
  resultVar <- newTVarIO Empty

  let close :: IO ()
      close =
        atomically (writeTVar resultVar Closed)

  uninterruptibleMask \_ -> do
    forkWithUnmask scope \unmask -> do
      unmask (produce resultVar action) `onException` close
      close

  pure do
    readTVar resultVar >>= \case
      Closed -> pure Nothing
      Empty -> retry
      Full value tookVar -> do
        writeTVar resultVar Empty
        putTMVar tookVar ()
        pure (Just value)

produce :: Context => TVar (S a) -> (Context => (a -> IO ()) -> IO ()) -> IO ()
produce resultVar action =
  action \value -> do
    tookVar <- newEmptyTMVarIO
    (join . atomically) do
      readTVar resultVar >>= \case
        Closed -> error "closed"
        Empty -> do
          writeTVar resultVar $! Full value tookVar
          pure (join (atomically (pure <$> readTMVar tookVar <|> cancelledSTM)))
        Full _ _ -> cancelledSTM

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
