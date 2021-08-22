module Ki.Timeout
  ( timeoutSTM,
  )
where

import Ki.Duration (Duration)
import qualified Ki.Duration
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
timeoutSTM duration action fallback = do
  (delay, unregister) <- liftIO (registerDelay (Ki.Duration.toMicroseconds duration))
  join (liftIO (atomically (delay $> fallback <|> (liftIO unregister >>) <$> action)))
{-# SPECIALIZE timeoutSTM :: Duration -> STM (IO a) -> IO a -> IO a #-}
