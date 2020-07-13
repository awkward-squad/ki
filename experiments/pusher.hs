{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad
import Data.Function
import Data.Word
import GHC.Clock
import qualified Ki.Implicit as Ki
import System.Environment
import Text.Printf

main :: IO ()
main = Ki.global do
  (n, m) <-
    getArgs >>= \case
      [read -> n, read -> m] -> pure (n, m)
      _ -> error "usage: pusher n m"

  Ki.scoped \scope -> do
    pull <-
      Ki.pusher scope \push -> do
        Ki.scoped \s -> do
          replicateM_ m (Ki.fork s (replicateM_ (n `div` m) (push ())))
          Ki.wait s
    ns <-
      timed do
        fix \again ->
          atomically pull >>= \case
            Nothing -> pure ()
            Just () -> again
    printf
      "(pusher) pushed %d from %d threads in %dns (%.2fns each)\n"
      n
      m
      ns
      (fromIntegral ns / fromIntegral n :: Double)

  var <- newEmptyMVar
  replicateM_ m (forkIO (replicateM_ (n `div` m) (putMVar var ())))
  ns <- timed (replicateM_ n (takeMVar var))
  printf
    "(mvar) pushed %d in from %d threads in %dns (%.2fns each)\n"
    n
    m
    ns
    (fromIntegral ns / fromIntegral n :: Double)

timed :: IO () -> IO Word64
timed action = do
  t0 <- getMonotonicTimeNSec
  action
  t1 <- getMonotonicTimeNSec
  pure (t1 - t0)
