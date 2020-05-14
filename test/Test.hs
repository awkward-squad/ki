{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent.Classy hiding (wait)
import Control.Exception (Exception (fromException), SomeAsyncException, SomeException)
import Control.Monad
import Data.Foldable
import Data.Function
import Data.List
import Data.Maybe (isJust)
import GHC.Clock
import Gy
import System.Exit
import qualified Test.DejaFu as DejaFu
import qualified Test.DejaFu.Types as DejaFu
import Text.Printf (printf)

main :: IO ()
main = do
  test "noop" . returns () $ do
    scoped background \_ -> pure ()

  test "using a closed scope throws an exception" . throws @ScopeClosed $ do
    scope <- scoped background pure
    void (async scope \_ -> pure ())

  test "join (-1) waits indefinitely for children" . returns True $ do
    ref <- newIORef False
    scoped background \scope -> do
      void (async scope \_ -> writeIORef ref True)
      wait scope
    readIORef ref

  test "child can be awaited" . returns () $ do
    scoped background \scope -> do
      child <- async scope \_ -> pure ()
      await child

  test "child can be awaited outside its scope" . returns () $ do
    child <- scoped background \scope -> do
      child <- async scope \_ -> pure ()
      wait scope
      pure child
    await child

  test "child can be cancelled" . returns () $ do
    scoped background \scope -> do
      child <- async scope \_ -> newEmptyMVar >>= takeMVar
      cancel child

  test "child can be cancelled after it's finished" . returns () $ do
    scoped background \scope -> do
      child <- async scope \_ -> pure ()
      await child
      cancel child

  test "dying child throws async exception first" . returns True $ do
    scoped background \scope ->
      mask \unmask -> do
        child <- async scope \_ -> throw A
        unmask (False <$ await child) `catch` \ex -> do
          pure (isAsyncException ex)

  test "failed child throws exception when awaited" . throws @ThreadFailed $ do
    var <- newEmptyMVar
    ignoring @ThreadFailed do
      scoped background \scope ->
        mask_ do
          child <- async scope \_ -> () <$ throw A
          putMVar var child
          wait scope
    child <- takeMVar var
    await child

  test "child can mask exceptions forever" . deadlocks $ do
    scoped background \scope -> do
      child <- asyncMasked scope \_ _ -> newEmptyMVar >>= takeMVar
      cancel child

  test "child can mask exceptions briefly" . returns True $ do
    ref <- newIORef False
    scoped background \scope -> do
      child <- asyncMasked scope \_ unmask ->
        unmask (pure ()) `finally` writeIORef ref True
      cancel child
      readIORef ref

  test "cancelling a child doesn't cancel its siblings" . returns True $ do
    ref <- newIORef False
    scoped background \scope -> do
      child <- async scope \_ -> newEmptyMVar >>= takeMVar
      void (async scope \_ -> writeIORef ref True)
      cancel child
      wait scope
    readIORef ref

  test "scope re-throws exceptions from children" . throws @ThreadFailed $ do
    scoped background \scope -> do
      void (async scope \_ -> () <$ throw A)
      wait scope

  test "scope cancels children when it dies" . returns True $ do
    ref <- newIORef False
    ignoring @A do
      scoped background \scope -> do
        void $ asyncMasked scope \_ unmask -> do
          unmask (pure ()) `finally` writeIORef ref True
        void (throw A)
    readIORef ref

  test "scope cancels children when it's cancelled" . returns True $ do
    ref <- newIORef False
    scoped background \scope1 -> do
      var <- newEmptyMVar
      child <-
        async scope1 \context ->
          scoped context \scope2 -> do
            void $ asyncMasked scope2 \_ unmask -> do
              putMVar var ()
              unmask (pure ()) `finally` writeIORef ref True
            wait scope2
      takeMVar var
      cancel child
      wait scope1
    readIORef ref

  test "scope cancels children when one dies" . returns True $ do
    ref <- newIORef False
    ignoring @ThreadFailed do
      scoped background \scope -> do
        void $ asyncMasked scope \_ unmask -> do
          var <- newEmptyMVar
          unmask (takeMVar var) `finally` writeIORef ref True
        void (async scope \_ -> () <$ throw A)
        wait scope
    readIORef ref

  test "scope can be joined immediately" . returns () $ do
    scoped background \scope -> waitFor scope 0

  test "child joining its own scope cancels self" . returns True $ do
    ref <- newIORef False
    scoped background \scope -> do
      void (async scope \_ -> waitFor scope 0 `onException` writeIORef ref True)
      wait scope
    readIORef ref

  test "child joining its own scope cancels siblings" . returns True $ do
    ref <- newIORef False
    scoped background \scope -> do
      void $ asyncMasked scope \_ unmask -> do
        var <- newEmptyMVar
        unmask (takeMVar var) `finally` writeIORef ref True
      void (async scope \_ -> waitFor scope 0)
      wait scope
    readIORef ref

type P =
  DejaFu.Program DejaFu.Basic IO

test :: Show a => String -> IO (DejaFu.Result a) -> IO ()
test name action = do
  time0 <- getMonotonicTime
  result <- action
  time1 <- getMonotonicTime
  printf
    "[%s] %4.0fms %s\n"
    (if DejaFu._pass result then "x" else " ")
    ((time1 - time0) * 1000)
    name
  for_ (DejaFu._failures result) \(value, trace) ->
    prettyPrintTrace value trace
  unless (DejaFu._pass result) exitFailure

runTest ::
  Eq a =>
  (DejaFu.Condition -> Bool) ->
  (a -> Bool) ->
  P a ->
  IO (DejaFu.Result a)
runTest p q =
  DejaFu.runTestWithSettings
    ( DejaFu.fromWayAndMemType
        ( DejaFu.systematically
            DejaFu.Bounds
              { DejaFu.boundPreemp = Just 20,
                DejaFu.boundFair = Just 10
              }
        )
        DejaFu.defaultMemType
    )
    (DejaFu.representative (DejaFu.alwaysTrue (either p q)))

returns :: Eq a => a -> P a -> IO (DejaFu.Result a)
returns expected =
  runTest (const False) (== expected)

throws :: forall e a. (Eq a, Exception e) => P a -> IO (DejaFu.Result a)
throws =
  runTest
    ( \case
        DejaFu.UncaughtException ex -> isJust (fromException @e ex)
        _ -> False
    )
    (const False)

deadlocks :: Eq a => P a -> IO (DejaFu.Result a)
deadlocks =
  runTest
    ( \case
        DejaFu.Deadlock -> True
        _ -> False
    )
    (const False)

ignoring :: forall e. Exception e => P () -> P ()
ignoring action =
  catch @_ @e action \_ -> pure ()

isAsyncException :: SomeException -> Bool
isAsyncException =
  isJust . fromException @SomeAsyncException

isSyncException :: SomeException -> Bool
isSyncException =
  not . isAsyncException

prettyPrintTrace :: Show a => Either DejaFu.Condition a -> DejaFu.Trace -> IO ()
prettyPrintTrace value trace = do
  print value
  flip fix trace \loop -> \case
    [] -> pure ()
    (decision, _, action) : xs -> do
      case decision of
        DejaFu.Start n -> putStrLn ("  [" ++ prettyThreadId n ++ "]")
        DejaFu.SwitchTo n -> putStrLn ("  [" ++ prettyThreadId n ++ "]")
        DejaFu.Continue -> pure ()
      putStrLn ("    " ++ prettyThreadAction action)
      loop xs

prettyThreadAction :: DejaFu.ThreadAction -> String
prettyThreadAction = \case
  DejaFu.BlockedSTM actions -> "atomically " ++ show actions ++ " (blocked)"
  DejaFu.BlockedTakeMVar n -> "takeMVar " ++ prettyMVarId n ++ " (blocked)"
  DejaFu.BlockedThrowTo n -> "throwTo " ++ prettyThreadId n ++ " (blocked)"
  DejaFu.Fork n -> "fork " ++ prettyThreadId n
  DejaFu.MyThreadId -> "myThreadId"
  DejaFu.NewIORef n -> prettyIORefId n ++ " <- newIORef"
  DejaFu.NewMVar n -> prettyMVarId n ++ " <- newMVar"
  DejaFu.PutMVar n [] -> "putMVar " ++ prettyMVarId n
  DejaFu.PutMVar n ts ->
    "putMVar " ++ prettyMVarId n ++ " (waking "
      ++ intercalate ", " (map prettyThreadId ts)
      ++ ")"
  DejaFu.ReadIORef n -> "readIORef " ++ prettyIORefId n
  DejaFu.ResetMasking _ state -> "setMaskingState " ++ show state
  DejaFu.Return -> "pure"
  DejaFu.STM actions _ -> "atomically " ++ show actions
  DejaFu.SetMasking _ state -> "setMaskingState " ++ show state
  DejaFu.Stop -> "stop"
  DejaFu.TakeMVar n [] -> "takeMVar " ++ prettyMVarId n
  DejaFu.TakeMVar n ts ->
    "takeMVar " ++ prettyMVarId n ++ " (waking "
      ++ intercalate ", " (map prettyThreadId ts)
      ++ ")"
  DejaFu.ThrowTo n success ->
    "throwTo " ++ prettyThreadId n
      ++ if success then " (killed)" else " (didn't kill)"
  action -> show action

prettyIORefId :: DejaFu.IORefId -> String
prettyIORefId n =
  "ioref#" ++ show n

prettyMVarId :: DejaFu.MVarId -> String
prettyMVarId n =
  "mvar#" ++ show n

prettyThreadId :: DejaFu.ThreadId -> String
prettyThreadId n =
  "thread#" ++ show n

data A
  = A
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

finally :: P a -> P b -> P a
finally action after =
  mask \restore -> do
    result <- restore action `onException` after
    _ <- after
    pure result

onException :: P a -> P b -> P a
onException action cleanup =
  catch @_ @SomeException action \ex -> do
    _ <- cleanup
    throw ex
