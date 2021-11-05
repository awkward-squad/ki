module Ki.Timeout
  ( timeoutSTM,
  )
where

import Ki.Duration (Duration(Micros))
import Ki.Prelude

-- | Wait for an @STM@ action to return an @m@ action, or if the given duration elapses, return the given @m@ action
-- instead.
timeoutSTM ::
  MonadIO m =>
  -- |
  Duration ->
  -- |
  STM (m a) ->
  -- |
  m a ->
  m a
timeoutSTM (Micros us) action fallback = do
  (delay, unregister) <- liftIO (registerDelay us)
  let lhs = do
        delay
        pure fallback
  let rhs = do
        io <- action
        pure do
          liftIO unregister
          io
  join (liftIO (atomically (lhs <|> rhs)))
{-# SPECIALIZE timeoutSTM :: Duration -> STM (IO a) -> IO a -> IO a #-}
