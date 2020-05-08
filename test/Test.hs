{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

-- import Control.Concurrent
import Control.Concurrent.Classy
import Control.Exception (AsyncException (ThreadKilled), Exception, SomeException)
import Control.Monad
import Control.Monad.IO.Class
-- import Data.IORef
import Data.Foldable
import Data.Function
import Data.Typeable (Typeable)
import GHC.Clock
import qualified Test.DejaFu as DejaFu
import qualified Test.DejaFu.Types as DejaFu
import Text.Printf (printf)
import Trio.Internal

main :: IO ()
main = do
  deterministic "empty" do
    withNursery \_ -> pure ()

  deterministic "waits for child" do
    ref <- newIORef False
    withNursery \nursery -> do
      forkChild_ nursery do
        writeIORef ref True
    readIORef ref

  deterministic "re-throws exceptions from children" do
    ref <- newIORef False
    catch @_ @(ChildDied (DejaFu.Program DejaFu.Basic IO))
      ( withNursery \nursery -> do
          forkChild_ nursery do
            throw A
      )
      (\_ -> writeIORef ref True)
    readIORef ref

  deterministic "cancels children when exceptions are thrown from parent" do
    var <- newEmptyMVar
    ref <- newIORef False
    _ <-
      try @A do
        withNursery \nursery -> do
          forkMaskedChild_ nursery \unmask -> do
            unmask (readMVar var) `onThreadKilled` writeIORef ref True
          _ <- throw A
          putMVar var ()
    readIORef ref

  deterministic "waits for children to finalize" do
    ref <- newIORef False
    _ <-
      try @A do
        withNursery \nursery -> do
          forkMaskedChild_ nursery \unmask -> do
            unmask (threadDelay 100) `finally` writeIORef ref True
          throw A
    readIORef ref

  deterministic "cancels children when exceptions are thrown from sibling" do
    var <- newEmptyMVar
    ref <- newIORef False
    _ <-
      try @(ChildDied (DejaFu.Program DejaFu.Basic IO)) do
        withNursery \nursery -> do
          forkMaskedChild_ nursery \unmask -> do
            unmask (readMVar var) `onException` writeIORef ref True
          forkChild_ nursery do
            throw A
        putMVar var ()
    readIORef ref

deterministic ::
  (Eq a, Show a) =>
  String ->
  DejaFu.Program DejaFu.Basic IO a ->
  IO ()
deterministic name action = do
  time0 <- getMonotonicTime
  result <- DejaFu.runTest (DejaFu.representative DejaFu.alwaysSame) action
  time1 <- getMonotonicTime
  printf
    "[%s] %4.0fms %s\n"
    (if DejaFu._pass result then "x" else " ")
    ((time1 - time0) * 1000)
    name
  unless (DejaFu._pass result) do
    for_ (DejaFu._failures result) \(value, trace) ->
      prettyPrintTrace value trace

prettyPrintTrace :: Show a => Either DejaFu.Condition a -> DejaFu.Trace -> IO ()
prettyPrintTrace value trace = do
  print value
  flip fix trace \loop -> \case
    [] -> pure ()
    (decision, _, action) : xs -> do
      case decision of
        DejaFu.Start tid -> putStrLn $ "  Start " ++ show tid
        DejaFu.SwitchTo tid -> putStrLn $ "  Switch to " ++ show tid
        DejaFu.Continue -> pure ()
      putStrLn $ "    " ++ show action
      loop xs

forkMaskedChild_ ::
  (MonadConc m, MonadIO m, Typeable m) =>
  Nursery m ->
  ((forall x. m x -> m x) -> m ()) ->
  m ()
forkMaskedChild_ nursery action =
  void (forkMaskedChild nursery action)

forkChild ::
  (MonadConc m, MonadIO m, Typeable m) =>
  Nursery m ->
  m () ->
  m (ThreadId m)
forkChild nursery action =
  forkMaskedChild nursery \unmask -> unmask action

forkChild_ ::
  (MonadConc m, MonadIO m, Typeable m) =>
  Nursery m ->
  m () ->
  m ()
forkChild_ nursery action =
  void (forkChild nursery action)

data A
  = A
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

testIO :: String -> IO Bool -> IO ()
testIO name action = do
  time0 <- getMonotonicTime
  result <- action
  time1 <- getMonotonicTime
  printf
    "[%s] %7.2fus %s\n"
    (if result then "x" else " ")
    ((time1 - time0) * 1000000)
    name

onThreadKilled :: MonadConc m => m a -> m b -> m a
onThreadKilled action cleanup =
  catch action \ex -> do
    when (ex == ThreadKilled) (void cleanup)
    throw ex

onException :: MonadConc m => m a -> m b -> m a
onException action cleanup =
  catch @_ @SomeException action \ex -> do
    _ <- cleanup
    throw ex

finally :: MonadConc m => m a -> m b -> m a
finally action cleanup =
  mask \unmask -> do
    result <- unmask action `onException` cleanup
    _ <- cleanup
    pure result

try :: (Exception e, MonadConc m) => m a -> m (Either e a)
try action =
  fmap Right action `catch` (pure . Left)
