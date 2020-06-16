{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Foldable
import Data.Traversable
import Data.Word
import GHC.Clock
import Control.Applicative
import Ki
import System.Environment (getArgs)
import Text.Printf
import Text.Read (readMaybe)
import qualified Data.Set as Set

main :: IO ()
main = Ki.global do
  [readMaybe -> Just n, readMaybe -> Just m, readMaybe -> Just o] <- getArgs

  var <- newEmptyMVar
  tids <- do
    (tids, us) <- timed (replicateM n (forkIO (readMVar var)))
    printf "%0.2fus forking %d threads\n" us n
    pure (Set.fromList tids)

  if m == 1 then do
    ((), us) <- timed (for_ tids killThread)
    printf "%0.02fus Killed %d threads from %d threads\n" us n m
  else do
    (results, us) <- timed do
      q <- newTQueueIO
      doneEnqueuingVar <- newTVarIO False
      scoped \scope -> do
        replicateM_ m $ fork scope $ do
          let loop = do
                join . atomically $ do
                  (do x <- readTQueue q
                      pure do
                        for_ x killThread
                        loop)
                    <|>
                    (do b <- readTVar doneEnqueuingVar
                        if b then pure (pure ()) else retry)
          loop
        let loop [] = atomically (writeTVar doneEnqueuingVar True)
            loop (t:ts) =
              if Set.size t > o then
                case Set.splitRoot t of
                  [xs, ys, zs] -> loop (xs : (ys <> zs) : ts)
              else do
                atomically (writeTQueue q t)
                loop ts
        loop [tids]
        wait scope
    printf "%0.02fus Killed %d threads from %d threads killing %d at a time\n" us n m o
    -- print results

timed :: IO a -> IO (a, Double)
timed action = do
  t0 <- getMonotonicTimeNSec
  result <- action
  t1 <- getMonotonicTimeNSec
  pure (result, fromIntegral (t1 - t0) / 1000)
