module Ki.Internal.Timeout
  ( timeoutSTM,
  )
where

import Ki.Internal.Duration (Duration)
import qualified Ki.Internal.Duration
import Ki.Internal.Prelude

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
  (delay, unregister) <- liftIO (registerDelay (Ki.Internal.Duration.toMicroseconds duration))
  join (liftIO (atomically (delay $> fallback <|> (liftIO unregister >>) <$> action)))
{-# SPECIALIZE timeoutSTM :: Duration -> STM (IO a) -> IO a -> IO a #-}
