module Ki.Timeout
  ( timeoutSTM,
  )
where

import Ki.Concurrency
import Ki.Prelude
import Ki.Seconds (Seconds)
import qualified Ki.Seconds as Seconds

-- | Wait for an @STM@ action to return, and return the @IO@ action contained within.
--
-- If the given number of seconds elapses, return the given @IO@ action instead.
timeoutSTM :: Seconds -> STM (IO a) -> IO a -> IO a
timeoutSTM seconds action fallback = do
  (delay, unregister) <- registerDelay (Seconds.toMicros seconds)
  join (atomically (delay $> fallback <|> (unregister >>) <$> action))
