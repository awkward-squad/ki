{-# LANGUAGE BlockArguments #-}

module Trio.Internal.Debug
  ( debug,
  )
where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Say
import System.Environment
import System.IO.Unsafe

debug :: MonadIO m => String -> m ()
debug message = liftIO do
  when shouldDebug do
    threadId <- myThreadId
    sayString ("[" ++ show threadId ++ "] " ++ message)

shouldDebug :: Bool
shouldDebug =
  unsafePerformIO (isJust <$> lookupEnv "TRIO_DEBUG")
{-# NOINLINE shouldDebug #-}
