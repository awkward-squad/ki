{-# LANGUAGE BlockArguments #-}

module Trio.Internal.Debug
  ( debug,
  )
where

import Control.Concurrent
import Control.Monad
import Data.Maybe
import Say
import System.Environment
import System.IO.Unsafe

debug :: Monad m => String -> m ()
debug message =
  when shouldDebug (unsafePerformIO output `seq` pure ())
  where
    output :: IO ()
    output = do
      threadId <- myThreadId
      sayString ("[" ++ show threadId ++ "] " ++ message)

shouldDebug :: Bool
shouldDebug =
  unsafePerformIO (isJust <$> lookupEnv "TRIO_DEBUG")
{-# NOINLINE shouldDebug #-}
