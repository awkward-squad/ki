module Ki.Timeout
  ( timeoutSTM,
  )
where

import Ki.Duration (Duration)
import qualified Ki.Duration
import Ki.Prelude

-- | Wait for an @STM@ action to return an @IO@ action, or if the given duration elapses, return the given @IO@ action
-- instead.
timeoutSTM :: Duration -> STM (IO a) -> IO a -> IO a
timeoutSTM duration action fallback = do
  (delay, unregister) <- registerDelay (Ki.Duration.toMicroseconds duration)
  atomicallyIO (delay $> fallback <|> (unregister >>) <$> action)
