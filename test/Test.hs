{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Concurrent.Classy hiding (wait)
import Control.Exception (Exception (fromException), MaskingState (..), SomeAsyncException, SomeException)
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
  test "the background context isn't cancelled" . returns False $ do
    cancelled background

  test "a new context isn't cancelled" . returns False $ do
    scoped background \scope -> async scope cancelled >>= await

  test "a context can be cancelled" . returns () $ do
    scoped background \scope -> do
      uninterruptibleMask_ do
        async_ scope \context ->
          atomically (retryUntilTrue (cancelledSTM context))
      waitFor scope 1

  todo "a context derived from a cancelled context is cancelled"

  test "can wait on a scope" . returns True $ do
    ref <- newIORef False
    scoped background \scope -> do
      async_ scope \_ -> writeIORef ref True
      wait scope
    readIORef ref

  test "using a closed scope throws an exception" . throws @ScopeClosed $ do
    scope <- scoped background pure
    async_ scope \_ -> pure ()

  test "thread can be awaited" . returns True $ do
    ref <- newIORef False
    scoped background \scope -> do
      thread <- async scope \_ -> writeIORef ref True
      await thread
    readIORef ref

  test "thread can be awaited after its scope closes" . returns () $ do
    thread <- scoped background \scope -> do
      thread <- async scope \_ -> pure ()
      wait scope
      pure thread
    await thread

  test "thread can be killed" . returns () $ do
    scoped background \scope -> do
      thread <- async scope \_ -> block
      kill thread

  test "thread can be killed after it's finished" . returns () $ do
    scoped background \scope -> do
      thread <- async scope \_ -> pure ()
      await thread
      kill thread

  test "failed thread throws async exception first" . returns True $ do
    scoped background \scope ->
      mask \restore -> do
        thread <- async scope \_ -> throw A
        restore (False <$ await thread) `catch` \ex ->
          pure (isAsyncException ex)

  test "failed thread throws exception when awaited" . throws @ThreadFailed $ do
    thread <- scoped background \scope -> async scope \_ -> block
    await thread

  test "thread forked with async inherits masking state" . returns (Unmasked, MaskedInterruptible, MaskedUninterruptible) $ do
    scoped background \scope -> do
      thread1 <- async scope \_ -> getMaskingState
      thread2 <- mask_ (async scope \_ -> getMaskingState)
      thread3 <- uninterruptibleMask_ (async scope \_ -> getMaskingState)
      (,,) <$> await thread1 <*> await thread2 <*> await thread3

  test "thread forked with asyncWithUnmask inherits masking state" . returns (Unmasked, MaskedInterruptible, MaskedUninterruptible) $ do
    scoped background \scope -> do
      thread1 <- asyncWithUnmask scope \_ _ -> getMaskingState
      thread2 <- mask_ (asyncWithUnmask scope \_ _ -> getMaskingState)
      thread3 <- uninterruptibleMask_ (asyncWithUnmask scope \_ _ -> getMaskingState)
      (,,) <$> await thread1 <*> await thread2 <*> await thread3

  test "asyncWithUnmask provides an unmasking function" . returns Unmasked $ do
    scoped background \scope -> do
      thread <- mask_ (asyncWithUnmask scope \_ unmask -> unmask getMaskingState)
      await thread

  test "killing a thread doesn't close its scope" . returns True $ do
    ref <- newIORef False
    scoped background \scope -> do
      thread <- async scope \_ -> block
      async_ scope \_ -> writeIORef ref True
      kill thread
      wait scope
    readIORef ref

  todo "scope waits for threads to finish before returning"

  test "scope re-throws exceptions from threads" . throws @ThreadFailed $ do
    scoped background \scope -> do
      async_ scope \_ -> () <$ throw A
      wait scope

  test "scope closes when it fails" . returns () $ do
    ignoring @A do
      scoped background \scope -> do
        async_ scope \_ -> block
        void (throw A)

  test "scope closes when a thread fails" . returns () $ do
    ignoring @ThreadFailed do
      scoped background \scope -> do
        async_ scope \_ -> block
        async_ scope \_ -> void (throw A)
        wait scope

  test "scope can be waited on for 0us" . returns () $ do
    scoped background \scope -> waitFor scope 0

  test "thread waiting indefinitely on its own scope fails" . returns True $ do
    ref <- newIORef False
    scoped background \scope -> do
      async_ scope \_ -> waitFor scope 0 `onException` writeIORef ref True
      wait scope
    readIORef ref

  test "thread waiting for 0us own its own scope closes it" . returns () $ do
    scoped background \scope -> do
      async_ scope \_ -> block
      async_ scope \_ -> waitFor scope 0
      wait scope

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

todo :: String -> IO ()
todo =
  printf "[ ] %4.0fms %s\n" (0 :: Float)

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

-- deadlocks :: Eq a => P a -> IO (DejaFu.Result a)
-- deadlocks =
--   runTest
--     ( \case
--         DejaFu.Deadlock -> True
--         _ -> False
--     )
--     (const False)

block :: P ()
block =
  newEmptyMVar >>= takeMVar

ignoring :: forall e. Exception e => P () -> P ()
ignoring action =
  catch @_ @e action \_ -> pure ()

isAsyncException :: SomeException -> Bool
isAsyncException =
  isJust . fromException @SomeAsyncException

retryUntilTrue :: MonadSTM m => m Bool -> m ()
retryUntilTrue action =
  action >>= \case
    False -> retry
    True -> pure ()

-- isSyncException :: SomeException -> Bool
-- isSyncException =
--   not . isAsyncException

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

-- finally :: P a -> P b -> P a
-- finally action after =
--   mask \restore -> do
--     result <- restore action `onException` after
--     _ <- after
--     pure result

onException :: P a -> P b -> P a
onException action cleanup =
  catch @_ @SomeException action \ex -> do
    _ <- cleanup
    throw ex
