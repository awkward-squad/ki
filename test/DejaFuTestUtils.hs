{-# LANGUAGE TypeApplications #-}

module DejaFuTestUtils
  ( P,
    _failingTest,
    block,
    deadlocks,
    ignoring,
    returns,
    test,
    throws,
    todo,
  )
where

import Control.Concurrent.Classy hiding (fork, forkWithUnmask, wait)
import Control.Exception (Exception (fromException))
import Control.Monad
import Data.Foldable
import Data.Function
import Data.List (intercalate)
import Data.Maybe
import Ki.Implicit
import System.Exit
import qualified Test.DejaFu as DejaFu
import qualified Test.DejaFu.Types as DejaFu
import Text.Printf (printf)
import Prelude

type P =
  DejaFu.Program DejaFu.Basic IO

test :: (Eq a, Show a) => String -> DejaFu.Predicate a -> (Context => P a) -> IO ()
test name predicate t = do
  result <- DejaFu.runTestWithSettings dejaFuSettings (DejaFu.representative predicate) (global t)
  printf "[%s] %s\n" (if DejaFu._pass result then "x" else " ") name
  for_ (DejaFu._failures result) \(value, trace) -> prettyPrintTrace value trace
  unless (DejaFu._pass result) exitFailure

_failingTest :: String -> IO (DejaFu.Result a) -> IO ()
_failingTest name action = do
  result <- action
  printf "[%s] %s\n" (if DejaFu._pass result then " " else "x") name
  when (DejaFu._pass result) exitFailure

todo :: String -> IO ()
todo =
  printf "[-] %s\n"

dejaFuSettings :: DejaFu.Settings IO a
dejaFuSettings =
  DejaFu.fromWayAndMemType (DejaFu.systematically bounds) DejaFu.defaultMemType
  where
    bounds =
      DejaFu.Bounds
        { DejaFu.boundPreemp = Just 2,
          DejaFu.boundFair = Just 5
        }

returns :: Eq a => a -> DejaFu.Predicate a
returns expected =
  DejaFu.alwaysTrue \case
    Left _ -> False
    Right actual -> actual == expected

throws :: (Eq e, Exception e) => e -> DejaFu.Predicate a
throws expected =
  DejaFu.alwaysTrue \case
    Left (DejaFu.UncaughtException actual) -> fromException actual == Just expected
    _ -> False

deadlocks :: DejaFu.Predicate a
deadlocks =
  DejaFu.alwaysTrue \case
    Left DejaFu.Deadlock -> True
    _ -> False

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
  DejaFu.Throw Nothing -> "throw (thread died)"
  DejaFu.Throw (Just _) -> "throw (thread still alive)"
  DejaFu.ThrowTo n Nothing -> "throwTo " ++ prettyThreadId n ++ " (killed it)"
  DejaFu.ThrowTo n (Just _) -> "throwTo " ++ prettyThreadId n ++ " (didn't kill it)"
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
