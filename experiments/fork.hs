{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Concurrent
import Control.Monad
import Data.Foldable
import Data.Word
import GHC.Clock
import qualified Ki
import System.Environment (getArgs)
import Text.Printf
import Text.Read (readMaybe)

-- ./experiments/fork 1000000 forkIO +RTS -N1 -qg
-- forkIO ~ 0.28us
--
-- ./experiments/fork 1000000 killThread +RTS -N1 -qg
-- killThread ~ 0.73us
--
-- ./experiments/fork 1000000 async +RTS -N1 -qg
-- async ~ 8.49us

main :: IO ()
main = Ki.global do
  (readMaybe -> Just n) : funcs <- getArgs

  when ("forkIO" `elem` funcs) do
    ((), us) <- timed (replicateM_ n (forkIO (pure ())))
    printf "forkIO ~ %0.2fus\n" (us / fromIntegral n)

  when ("killThread" `elem` funcs) do
    var <- newEmptyMVar
    ids <- replicateM n (forkIO (readMVar var))
    ((), us) <- timed (for_ ids killThread)
    printf "killThread ~ %0.2fus\n" (us / fromIntegral n)
    putMVar var ()

  when ("scoped" `elem` funcs) do
    ((), us) <- timed (replicateM_ n (Ki.scoped \_ -> pure ()))
    printf "scoped ~ %0.2fus\n" (us / fromIntegral n)

  when ("async" `elem` funcs) do
    Ki.scoped \scope -> do
      ((), us) <- timed (replicateM_ n (Ki.async scope (pure ())))
      printf "async ~ %0.2fus\n" (us / fromIntegral n)

  when ("fork" `elem` funcs) do
    Ki.scoped \scope -> do
      ((), us) <- timed (replicateM_ n (Ki.fork scope (pure ())))
      printf "fork ~ %0.2fus\n" (us / fromIntegral n)

timed :: IO a -> IO (a, Double)
timed action = do
  t0 <- getMonotonicTimeNSec
  result <- action
  t1 <- getMonotonicTimeNSec
  pure (result, fromIntegral (t1 - t0) / 1000)
