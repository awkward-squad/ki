{-# LANGUAGE TypeApplications #-}

module TestUtils
  ( fail,
    shouldReturn,
    shouldReturnSuchThat,
    shouldThrow,
    shouldThrowSuchThat,
    test,
  )
where

import Control.Exception
import Control.Monad (unless)
import Data.Either (isRight)
import qualified Ki.Implicit as Ki
import System.Exit (exitFailure)
import Text.Printf (printf)
import Prelude hiding (fail)

newtype TestFailure
  = TestFailure String
  deriving stock (Show)

instance Exception TestFailure where
  displayException (TestFailure message) =
    message

fail :: String -> IO ()
fail =
  throwIO . TestFailure

shouldReturn :: (Eq a, Show a) => IO a -> a -> IO ()
shouldReturn action expected = do
  actual <- action
  unless (actual == expected) (fail ("expected " ++ show expected ++ ", got " ++ show actual))

shouldReturnSuchThat :: Show a => IO a -> (a -> Bool) -> IO ()
shouldReturnSuchThat action predicate = do
  result <- action
  unless (predicate result) (fail ("result " ++ show result ++ " did not pass predicate"))

shouldThrow :: (Show a, Eq e, Exception e) => IO a -> e -> IO ()
shouldThrow action expected =
  try @SomeException action >>= \case
    Left exception | fromException exception == Just expected -> pure ()
    Left exception ->
      fail ("expected exception " ++ displayException expected ++ ", got exception " ++ displayException exception)
    Right value -> fail ("expected exception " ++ displayException expected ++ ", got " ++ show value)

shouldThrowSuchThat :: (Show a, Exception e) => IO a -> (e -> Bool) -> IO ()
shouldThrowSuchThat action predicate =
  try @SomeException action >>= \case
    Left exception ->
      case fromException exception of
        Nothing ->
          fail ("expected exception, got exception " ++ displayException exception)
        Just exception' ->
          unless
            (predicate exception')
            (fail ("exception " ++ displayException exception' ++ " did not pass predicate"))
    Right value -> fail ("expected exception, got " ++ show value)

test :: String -> (Ki.Context => IO ()) -> IO ()
test name action = do
  result <- try @SomeException (Ki.withGlobalContext action)
  printf "[%s] %s\n" (if isRight result then "x" else " ") name
  case result of
    Left exception -> do
      putStrLn (displayException exception)
      exitFailure
    Right () -> pure ()
