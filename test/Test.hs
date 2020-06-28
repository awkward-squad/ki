{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Concurrent.Classy hiding (fork, forkWithUnmask, wait)
import Control.Exception (Exception (fromException), MaskingState (..), pattern ErrorCall)
import Control.Monad
import Data.Foldable
import Data.Function
import Data.Functor
import Data.List (intercalate)
import Data.Maybe
import GHC.Clock
import Ki.Implicit
import System.Exit
import qualified Test.DejaFu as DejaFu
import qualified Test.DejaFu.Types as DejaFu
import Text.Printf (printf)
import Prelude

main :: IO ()
main = do
  test "background context isn't cancelled" do
    returns False do
      isJust <$> cancelled

  test "scope doesn't start out cancelled" do
    returns False do
      scoped \_ ->
        isJust <$> cancelled

  test "`cancel` cancels scope" do
    returns True do
      scoped \scope -> do
        cancel scope
        isJust <$> cancelled

  test "`cancel` cancels inner scope" do
    returns True do
      scoped \scope1 -> do
        scoped \_ -> do
          cancel scope1
          isJust <$> cancelled

  test "`cancel` cancels inner thread" do
    returns True do
      scoped \scope1 -> do
        thread <-
          async scope1 do
            cancel scope1
            isJust <$> cancelled
        await' thread

  test "inner scope inherits cancellation" do
    returns True do
      scoped \scope1 -> do
        cancel scope1
        scoped \_ ->
          isJust <$> cancelled

  test "inner thread inherits cancellation" do
    returns True do
      scoped \scope -> do
        cancel scope
        thread <- async scope cancelled
        isJust <$> await' thread

  todo "cancelled child context removes parent's ref to it"

  test "`wait` waits for `fork`" do
    returns True do
      ref <- newIORef False
      scoped \scope -> do
        fork scope (writeIORef ref True)
        wait scope
      readIORef ref

  test "`wait` waits for `async`" do
    returns True do
      ref <- newIORef False
      scoped \scope -> do
        _ <- async scope (writeIORef ref True)
        wait scope
      readIORef ref

  test "using a closed scope throws" do
    throws (ErrorCall "ki: scope closed") do
      scope <- scoped pure
      fork scope (pure ())

  test "`await` waits" do
    returns True do
      scoped \scope -> do
        thread <- async scope (pure ())
        isRight <$> await thread

  test "`await` waits for exception" do
    returns True do
      scoped \scope -> do
        thread <- async scope (throw A)
        isLeft <$> await thread

  test "thread can be awaited after its scope closes" do
    returns True do
      thread <- scoped \scope -> do
        thread <- async scope (pure ())
        wait scope
        pure thread
      isRight <$> await thread

  test "thread can be killed" do
    returns () do
      scoped \scope -> do
        thread <- async scope block
        kill thread

  test "thread can be killed after it's finished" do
    returns () do
      scoped \scope -> do
        thread <- async scope (pure ())
        _ <- await thread
        kill thread

  test "`fork` propagates exceptions" do
    throws A do
      scoped \scope -> do
        fork scope (throw A)
        wait scope

  test "`async` doesn't propagate exceptions" do
    returns () do
      scoped \scope ->
        void (async scope (throw A))

  -- I think this deadlocks because of https://github.com/barrucadu/dejafu/issues/324
  -- Still, we want to assert either it succeeds or throws A
  --
  --   scoped \scope -> do
  --     fork scope (throw A)

  test "`await` returns Left if thread throws" do
    returns True do
      scoped \scope -> do
        thread <- async scope (throw A)
        isLeft <$> await thread

  test "`async` inherits masking state" do
    returns (Unmasked, MaskedInterruptible, MaskedUninterruptible) do
      scoped \scope -> do
        thread1 <- async scope getMaskingState
        thread2 <- mask_ (async scope getMaskingState)
        thread3 <- uninterruptibleMask_ (async scope getMaskingState)
        (,,)
          <$> (either throw pure =<< await thread1)
          <*> (either throw pure =<< await thread2)
          <*> (either throw pure =<< await thread3)

  test "`asyncWithUnmask` inherits masking state" do
    returns (Unmasked, MaskedInterruptible, MaskedUninterruptible) do
      scoped \scope -> do
        thread1 <- asyncWithUnmask scope \_ -> getMaskingState
        thread2 <- mask_ (asyncWithUnmask scope \_ -> getMaskingState)
        thread3 <- uninterruptibleMask_ (asyncWithUnmask scope \_ -> getMaskingState)
        (,,)
          <$> (either throw pure =<< await thread1)
          <*> (either throw pure =<< await thread2)
          <*> (either throw pure =<< await thread3)

  test "`asyncWithUnmask` provides an unmasking function" do
    returns Unmasked do
      scoped \scope -> do
        thread <- mask_ (asyncWithUnmask scope \unmask -> unmask getMaskingState)
        either throw pure =<< await thread

  todo "`forkWithUnmask` inherits masking state"

  todo "`forkWithUnmask` provides an unmasking function"

  test "`scoped` re-throws from `fork`" do
    throws A do
      scoped \scope -> do
        fork scope (() <$ throw A)
        wait scope

  test "`scoped` kills threads when it throws" do
    returns () do
      ignoring @A do
        scoped \scope -> do
          var <- newEmptyMVar
          uninterruptibleMask_ do
            forkWithUnmask scope \unmask -> do
              putMVar var ()
              unmask block
          takeMVar var
          void (throw A)

  test "`scoped` kills threads when `fork` throws" do
    returns () do
      ignoring @A do
        scoped \scope -> do
          fork scope block
          fork scope (void (throw A))
          wait scope

  test "thread waiting on its own scope deadlocks" do
    deadlocks do
      scoped \scope -> do
        fork scope (wait scope)
        wait scope

  test "thread waiting on its own scope allows async exceptions" do
    returns () do
      scoped \scope ->
        fork scope (wait scope)

  test "`fork` doesn't propagate `Cancelled`" do
    returns () do
      scoped \scope -> do
        cancel scope
        fork scope do
          cancelled >>= \case
            Nothing -> throw A
            Just capitulate -> capitulate
        wait scope

  test "`cancelled` returns an action that throws `Cancelled`" do
    returns True do
      scoped \scope -> do
        cancel scope
        thread <-
          async scope do
            cancelled >>= \case
              Nothing -> throw A
              Just capitulate ->
                (capitulate $> False) `catch` \case
                  Cancelled -> pure True
        await' thread

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

_failingTest :: String -> IO (DejaFu.Result a) -> IO ()
_failingTest name action = do
  time0 <- getMonotonicTime
  result <- action
  time1 <- getMonotonicTime
  printf
    "[%s] %4.0fms %s\n"
    (if DejaFu._pass result then " " else "x")
    ((time1 - time0) * 1000)
    name
  when (DejaFu._pass result) exitFailure

todo :: String -> IO ()
todo =
  printf "[-] %4.0fms %s\n" (0 :: Float)

runTest ::
  Eq a =>
  (DejaFu.Condition -> Bool) ->
  (a -> Bool) ->
  (Context => P a) ->
  IO (DejaFu.Result a)
runTest p q t =
  DejaFu.runTestWithSettings
    ( DejaFu.fromWayAndMemType
        ( DejaFu.systematically
            DejaFu.Bounds
              { DejaFu.boundPreemp = Just 2,
                DejaFu.boundFair = Just 5
              }
        )
        DejaFu.defaultMemType
    )
    (DejaFu.representative (DejaFu.alwaysTrue (either p q)))
    (global t)

returns :: Eq a => a -> (Context => P a) -> IO (DejaFu.Result a)
returns expected =
  runTest (const False) (== expected)

throws :: (Eq a, Eq e, Exception e) => e -> (Context => P a) -> IO (DejaFu.Result a)
throws expected =
  runTest
    ( \case
        DejaFu.UncaughtException ex -> fromException ex == Just expected
        _ -> False
    )
    (const False)

deadlocks :: Eq a => (Context => P a) -> IO (DejaFu.Result a)
deadlocks =
  runTest
    ( \case
        DejaFu.Deadlock -> True
        _ -> False
    )
    (const False)

block :: P ()
block =
  newEmptyMVar >>= takeMVar

ignoring :: forall e. Exception e => P () -> P ()
ignoring action =
  catch @_ @e action \_ -> pure ()

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
  DejaFu.Throw _ True -> "throw (thread died)"
  DejaFu.Throw _ False -> "throw (thread still alive)"
  DejaFu.ThrowTo n _ success -> "throwTo " ++ prettyThreadId n ++ if success then " (killed it)" else " (didn't kill it)"
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

await' :: Thread a -> P a
await' =
  await >=> either throw pure

isLeft :: Either a b -> Bool
isLeft =
  either (const True) (const False)

isRight :: Either a b -> Bool
isRight =
  either (const False) (const True)

-- finally :: P a -> P b -> P a
-- finally action after =
--   mask \restore -> do
--     result <- restore action `onException` after
--     _ <- after
--     pure result

-- onException :: P a -> P b -> P a
-- onException action cleanup =
--   catch @_ @SomeException action \ex -> do
--     _ <- cleanup
--     throw ex
