module Ki.Experimental.Producer
  ( producer,
  )
where

import Ki.Concurrency
import Ki.Implicit.Context
import Ki.Implicit.Fork
import Ki.Implicit.Scope
import Ki.Prelude

data S a
  = Closed
  | Empty
  | Full !a !(TMVar ())

producer :: Scope -> IO (Maybe a) -> IO (STM (Maybe a))
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
        putTMVar tookVar ()
        pure (Just value)

produce :: Context => TVar (S a) -> IO (Maybe a) -> IO ()
produce resultVar action =
  fix \again ->
    cancelled >>= \case
      Nothing ->
        action >>= \case
          Nothing -> pure ()
          Just value -> do
            tookVar <- newEmptyTMVarIO
            atomically (writeTVar resultVar (Full value tookVar))
            join (atomically (readTMVar tookVar $> again <|> cancelledSTM $> pure ()))
      Just _capitulate -> pure ()
