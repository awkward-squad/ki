module Main (main) where

import Control.Concurrent.STM (atomically)
import Control.Exception
import Control.Monad
import qualified Ki
import Test.Tasty
import Test.Tasty.HUnit
import Prelude

main :: IO ()
main =
  defaultMain do
    testGroup
      "Unit tests"
      [ testCase "`fork` throws ErrorCall when the scope is closed" do
          scope <- Ki.scoped pure
          (atomically . Ki.await =<< Ki.fork scope (pure ())) `shouldThrow` ErrorCall "ki: scope closed"
          pure (),
        testCase "`wait` succeeds when no threads are alive" do
          Ki.scoped (atomically . Ki.wait),
        testCase "`fork` propagates exceptions" do
          (`shouldThrow` A) do
            Ki.scoped \scope -> do
              Ki.fork_ scope (throwIO A)
              atomically (Ki.wait scope),
        testCase "`fork` puts exceptions after propagating" do
          (`shouldThrow` A) do
            Ki.scoped \scope -> do
              mask \restore -> do
                thread :: Ki.Thread () <- Ki.fork scope (throwIO A)
                restore (atomically (Ki.wait scope)) `catch` \(e :: SomeException) -> print e
                atomically (Ki.await thread),
        testCase "`fork` forks in unmasked state regardless of parent's masking state" do
          Ki.scoped \scope -> do
            _ <- Ki.fork scope (getMaskingState `shouldReturn` Unmasked)
            _ <- mask_ (Ki.fork scope (getMaskingState `shouldReturn` Unmasked))
            _ <- uninterruptibleMask_ (Ki.fork scope (getMaskingState `shouldReturn` Unmasked))
            atomically (Ki.wait scope),
        testCase "`forkWith` can fork in interruptibly masked state regardless of paren't masking state" do
          Ki.scoped \scope -> do
            _ <-
              Ki.forkWith
                scope
                Ki.defaultThreadOptions {Ki.maskingState = MaskedInterruptible}
                (getMaskingState `shouldReturn` MaskedInterruptible)
            _ <-
              mask_ do
                Ki.forkWith
                  scope
                  Ki.defaultThreadOptions {Ki.maskingState = MaskedInterruptible}
                  (getMaskingState `shouldReturn` MaskedInterruptible)
            _ <-
              uninterruptibleMask_ do
                Ki.forkWith
                  scope
                  Ki.defaultThreadOptions {Ki.maskingState = MaskedInterruptible}
                  (getMaskingState `shouldReturn` MaskedInterruptible)
            atomically (Ki.wait scope),
        testCase "`forkWith` can fork in uninterruptibly masked state regardless of paren't masking state" do
          Ki.scoped \scope -> do
            _ <-
              Ki.forkWith
                scope
                Ki.defaultThreadOptions {Ki.maskingState = MaskedUninterruptible}
                (getMaskingState `shouldReturn` MaskedUninterruptible)
            _ <-
              mask_ do
                Ki.forkWith
                  scope
                  Ki.defaultThreadOptions {Ki.maskingState = MaskedUninterruptible}
                  (getMaskingState `shouldReturn` MaskedUninterruptible)
            _ <-
              uninterruptibleMask_ do
                Ki.forkWith
                  scope
                  Ki.defaultThreadOptions {Ki.maskingState = MaskedUninterruptible}
                  (getMaskingState `shouldReturn` MaskedUninterruptible)
            atomically (Ki.wait scope),
        testCase "`forktry` can catch sync exceptions" do
          Ki.scoped \scope -> do
            result :: Ki.Thread (Either A ()) <- Ki.forktry scope (throw A)
            atomically (Ki.await result) `shouldReturn` Left A,
        testCase "`forktry` can propagate sync exceptions" do
          (`shouldThrow` A) do
            Ki.scoped \scope -> do
              thread :: Ki.Thread (Either A2 ()) <- Ki.forktry scope (throw A)
              atomically (Ki.await thread),
        testCase "`forktry` propagates async exceptions" do
          (`shouldThrow` B) do
            Ki.scoped \scope -> do
              thread :: Ki.Thread (Either B ()) <- Ki.forktry scope (throw B)
              atomically (Ki.await thread),
        testCase "`forktry` puts exceptions after propagating" do
          (`shouldThrow` A2) do
            Ki.scoped \scope -> do
              mask \restore -> do
                thread :: Ki.Thread (Either A ()) <- Ki.forktry scope (throwIO A2)
                restore (atomically (Ki.wait scope)) `catch` \(_ :: SomeException) -> pure ()
                atomically (Ki.await thread)
      ]

data A = A
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

data A2 = A2
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

data B = B
  deriving stock (Eq, Show)

instance Exception B where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

shouldReturn :: (Eq a, Show a) => IO a -> a -> IO ()
shouldReturn action expected = do
  actual <- action
  unless (actual == expected) (fail ("expected " ++ show expected ++ ", got " ++ show actual))

shouldThrow :: (Show a, Eq e, Exception e) => IO a -> e -> IO ()
shouldThrow action expected =
  try @SomeException action >>= \case
    Left exception | fromException exception == Just expected -> pure ()
    Left exception ->
      fail ("expected exception " ++ displayException expected ++ ", got exception " ++ displayException exception)
    Right value -> fail ("expected exception " ++ displayException expected ++ ", got " ++ show value)
