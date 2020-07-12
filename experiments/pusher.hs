{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Concurrent.STM
import Control.Monad
import Data.Function
import Data.Word
import GHC.Clock
import qualified Ki.Implicit as Ki
import Text.Printf
import Control.Concurrent
import Control.Concurrent.MVar

main :: IO ()
main = Ki.global do
  let n = 1000000
  let m = 100

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
    printf "(pusher) pulled %d in %dns (%.2fns each)\n" n ns (fromIntegral ns / fromIntegral n :: Double)

  var <- newEmptyMVar
  replicateM_ m (forkIO (replicateM_ (n `div` m) (putMVar var ())))
  ns <- timed (replicateM_ n (takeMVar var))
  printf "(mvar) pulled %d in %dns (%.2fns each)\n" n ns (fromIntegral ns / fromIntegral n :: Double)

timed :: IO () -> IO Word64
timed action = do
  t0 <- getMonotonicTimeNSec
  action
  t1 <- getMonotonicTimeNSec
  pure (t1 - t0)
