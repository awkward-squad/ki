module Ki.Timeout
  ( timeoutSTM,
  )
where

import Ki.Prelude
import Ki.Duration (Duration)
import qualified Ki.Duration

-- | Wait for an @STM@ action to return, and return the @IO@ action contained within.
--
-- If the given duration elapses, return the given @IO@ action instead.
timeoutSTM :: Duration -> STM (IO a) -> IO a -> IO a
timeoutSTM duration action fallback = do
  (delay, unregister) <- registerDelay (Ki.Duration.toMicroseconds duration)
  atomicallyIO (delay $> fallback <|> (unregister >>) <$> action)
