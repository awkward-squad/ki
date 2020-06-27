module Ki.Internal.Debug
  ( debug,
  )
where

import Ki.Internal.Concurrency
import Ki.Internal.Prelude
import System.IO.Unsafe (unsafePerformIO)

debug :: Monad m => String -> m ()
debug message =
  unsafePerformIO output `seq` pure ()
  where
    output :: IO ()
    output = do
      threadId <- myThreadId
      withMVar lock \_ -> putStrLn ("[" ++ show threadId ++ "] " ++ message)

lock :: MVar ()
lock =
  unsafePerformIO (newMVar ())
{-# NOINLINE lock #-}
