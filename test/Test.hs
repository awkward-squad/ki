{-# LANGUAGE AllowAmbiguousTypes #-}
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

import Control.Concurrent.Classy
import Control.Exception (Exception (fromException), SomeAsyncException, SomeException)
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.Function
import Data.List
import Data.Maybe (isJust)
import Data.Typeable (Typeable)
import GHC.Clock
import qualified Test.DejaFu as DejaFu
import qualified Test.DejaFu.Types as DejaFu
import Text.Printf (printf)
import Trio.Internal

main :: IO ()
main = do
  test "noop" do
    returns () (withScope \_ -> pure ())

  test "using a closed scope throws an exception" do
    throws @ScopeClosed do
      scope <- withScope pure
      async_ scope (pure ())

  test "scope waits for children on close" do
    returns True do
      ref <- newIORef False
      withScope \scope -> async_ scope (writeIORef ref True)
      readIORef ref

  test "a child can be awaited" do
    returns () do
      withScope \scope -> do
        child <- async scope (pure ())
        atomically (await child)

  test "a child can be awaited outside its scope" do
    returns () do
      child <- withScope \scope -> async scope (pure ())
      atomically (await child)

  test "a child can be canceled" do
    returns () do
      withScope \scope -> do
        var <- newEmptyMVar
        child <- async scope (takeMVar var)
        cancel child
        putMVar var ()

  test "a child can be canceled after it's finished" do
    returns () do
      withScope \scope -> do
        child <- async scope (pure ())
        atomically (await child)
        cancel child

  test "a dying child delivers an async exception first" do
    returns True do
      withScope \scope ->
        mask \unmask -> do
          child <- async scope (throw A)
          unmask (False <$ atomically (await child)) `catch` \ex -> do
            pure (isAsyncException ex)

  test "a failed child delivers a sync exception when awaited" do
    throws @(ChildDied P) do
      var <- newEmptyMVar
      ignoring @(ChildDied P) do
        withScope \scope ->
          mask_ do
            child <- async scope (() <$ throw A)
            putMVar var child
      child <- readMVar var
      atomically (await child)

  test "a child can mask exceptions forever" do
    deadlocks do
      withScope \scope -> do
        var <- newEmptyMVar
        child <- asyncMasked scope \_ -> takeMVar var
        cancel child
        putMVar var ()

  test "a child can mask exceptions briefly" do
    returns True do
      ref <- newIORef False
      withScope \scope -> do
        child <- asyncMasked scope \unmask ->
          unmask (pure ()) `finally` writeIORef ref True
        cancel child
        readIORef ref

  test "cancelling a child doesn't cancel its siblings" do
    returns True do
      ref <- newIORef False
      withScope \scope -> do
        var <- newEmptyMVar
        child <- async scope (takeMVar var)
        async_ scope (writeIORef ref True)
        cancel child
        putMVar var ()
      readIORef ref

  test "scope re-throws exceptions from children" do
    throws @(ChildDied P) (withScope \scope -> async_ scope (throw A))

  test "scope cancels children when it dies" do
    returns True do
      ref <- newIORef False
      ignoring @A do
        withScope \scope -> do
          asyncMasked_ scope \unmask -> do
            unmask (pure ()) `finally` writeIORef ref True
          void (throw A)
      readIORef ref

  test "scope cancels children when it's cancelled" do
    returns True do
      ref <- newIORef False
      withScope \scope1 -> do
        var <- newEmptyMVar
        child <-
          async scope1 do
            withScope \scope2 -> do
              asyncMasked_ scope2 \unmask -> do
                putMVar var ()
                unmask (pure ()) `finally` writeIORef ref True
        takeMVar var
        cancel child
      readIORef ref

  test "scope cancels children when one dies" do
    returns True do
      ref <- newIORef False
      ignoring @(ChildDied P) do
        var <- newEmptyMVar
        withScope \scope -> do
          asyncMasked_ scope \unmask ->
            unmask (takeMVar var) `finally` writeIORef ref True
          async_ scope (throw A)
        putMVar var ()
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

returns :: (Eq a, Show a) => a -> P a -> IO (DejaFu.Result a)
returns expected =
  DejaFu.runTest (DejaFu.representative (DejaFu.alwaysTrue p))
  where
    p = either (const False) (== expected)

throws :: forall e a. (Eq a, Exception e) => P a -> IO (DejaFu.Result a)
throws =
  DejaFu.runTest (DejaFu.representative (DejaFu.alwaysTrue p))
  where
    p =
      either
        ( \case
            DejaFu.UncaughtException ex -> isJust (fromException @e ex)
            _ -> False
        )
        (const False)

deadlocks :: Eq a => P a -> IO (DejaFu.Result a)
deadlocks =
  DejaFu.runTest (DejaFu.representative DejaFu.deadlocksAlways)

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
      case prettyThreadAction action of
        "" -> pure ()
        s -> putStrLn ("    " ++ s)
      loop xs

prettyThreadAction :: DejaFu.ThreadAction -> String
prettyThreadAction = \case
  DejaFu.BlockedSTM actions -> "atomically " ++ show actions ++ " (blocked)"
  DejaFu.BlockedTakeMVar n -> "takeMVar " ++ prettyMVarId n ++ " (blocked)"
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
  DejaFu.Stop -> ""
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

asyncMasked_ ::
  (MonadConc m, MonadIO m, Typeable m) =>
  TMVar (STM m) (Scope m) ->
  ((forall x. m x -> m x) -> m ()) ->
  m ()
asyncMasked_ scope action =
  void (asyncMasked scope action)

async ::
  (MonadConc m, MonadIO m, Typeable m) =>
  TMVar (STM m) (Scope m) ->
  m a ->
  m (Async m a)
async scope action =
  asyncMasked scope \unmask -> unmask action

async_ ::
  (MonadConc m, MonadIO m, Typeable m) =>
  TMVar (STM m) (Scope m) ->
  m a ->
  m ()
async_ scope action =
  void (async scope action)

data A
  = A
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

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
