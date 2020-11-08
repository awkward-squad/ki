{-# LANGUAGE TypeApplications #-}

module TestUtils
  ( fail,
    shouldReturn,
    test,
  )
where

import Control.Exception
import Control.Monad (unless)
import Data.Either (isRight)
import qualified Ki.Implicit as Ki
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

test :: String -> (Ki.Context => IO ()) -> IO ()
test name action = do
  result <- try @SomeException (Ki.withGlobalContext action)
  printf "[%s] %s\n" (if isRight result then "x" else " ") name
  case result of
    Left exception -> putStrLn (displayException exception)
    Right () -> pure ()
