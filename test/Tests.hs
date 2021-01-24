{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (when)
import Data.Functor
import Data.IORef
import Data.Maybe
import qualified Ki.Implicit as Ki
import qualified Ki.Internal
import TestUtils
import Prelude hiding (fail)

main :: IO ()
main = do
  test "background context isn't cancelled" do
    (isJust <$> Ki.cancelled) `shouldReturn` False

  test "new scope doesn't start out cancelled" do
    Ki.scoped_ \_ -> (isJust <$> Ki.cancelled) `shouldReturn` False

  test "`cancel` observable by scope's `cancelled`" do
    Ki.scoped_ \scope -> do
      Ki.cancel scope
      (isJust <$> Ki.cancelled) `shouldReturn` True

  test "`cancel` observable by inner scope's `cancelled`" do
    Ki.scoped_ \scope ->
      Ki.scoped_ \_ -> do
        Ki.cancel scope
        (isJust <$> Ki.cancelled) `shouldReturn` True

  childtest "`cancel` observable by child's `cancelled`" \fork -> do
    ref <- newIORef False
    Ki.scoped_ \scope -> do
      fork scope do
        Ki.cancel scope
        Ki.cancelled >>= writeIORef ref . isJust
      Ki.wait scope
    readIORef ref `shouldReturn` True

  childtest "`cancel` observable by grandchild's `cancelled`" \fork -> do
    ref <- newIORef False
    Ki.scoped_ \scope1 -> do
      fork scope1 do
        Ki.scoped_ \scope2 -> do
          fork scope2 do
            Ki.cancel scope1
            Ki.cancelled >>= writeIORef ref . isJust
          Ki.wait scope2
      Ki.wait scope1
    readIORef ref `shouldReturn` True

  test "inner scope inherits cancellation" do
    Ki.scoped_ \scope1 -> do
      Ki.cancel scope1
      Ki.scoped_ \_ -> (isJust <$> Ki.cancelled) `shouldReturn` True

  childtest "child thread inherits cancellation" \fork -> do
    ref <- newIORef False
    Ki.scoped_ \scope -> do
      Ki.cancel scope
      fork scope (Ki.cancelled >>= writeIORef ref . isJust)
      Ki.wait scope
    readIORef ref `shouldReturn` True

  childtest "creating a child thread throws ErrorCall when the scope is closed" \fork -> do
    scope <- Ki.scoped_ pure
    fork scope (pure ()) `shouldThrow` ErrorCall "ki: scope closed"

  test "closing child scope removes its context's parent's ref to it" do
    Ki.scoped_ \scope -> do
      let count = length <$> readTVarIO (Ki.Internal.context'childrenVar (Ki.Internal.scope'context scope))
      Ki.scoped_ \_ -> count `shouldReturn` 1
      count `shouldReturn` 0

  test "`wait` succeeds when no threads are alive" do
    Ki.scoped_ Ki.wait

  childtest "creates a thread" \fork -> do
    parentThreadId <- myThreadId
    Ki.scoped_ \scope -> do
      fork scope do
        childThreadId <- myThreadId
        when (parentThreadId == childThreadId) (fail "didn't create a thread")
      Ki.wait scope

  forktest "propagates sync exceptions" \fork ->
    ( Ki.scoped_ \scope -> do
        fork scope (throwIO A)
        Ki.wait scope
    )
      `shouldThrow` A

  forktest "propagates async exceptions" \fork ->
    ( Ki.scoped_ \scope -> do
        fork scope (throwIO B)
        Ki.wait scope
    )
      `shouldThrow` B

  forktest "doesn't propagate own CancelToken" \fork ->
    Ki.scoped_ \scope -> do
      Ki.cancel scope
      fork scope (atomically Ki.cancelledSTM >>= throwIO)
      Ki.wait scope

  forktest "doesn't propagate ancestor's CancelToken" \fork -> do
    Ki.scoped_ \scope0 -> do
      Ki.cancel scope0
      Ki.scoped_ \scope1 -> do
        fork scope1 (atomically Ki.cancelledSTM >>= throwIO)
        Ki.wait scope1

  forktest "propagates ScopeClosing if it isn't ours" \fork ->
    ( Ki.scoped_ \scope -> do
        fork scope (throwIO Ki.Internal.ScopeClosing)
        Ki.wait scope
    )
      `shouldThrow` Ki.Internal.ScopeClosing

  test "`async` returns sync exceptions" do
    Ki.scoped_ \scope -> do
      result <- Ki.async @_ @() scope (throw A)
      Ki.await result `shouldReturnSuchThat` \case
        Left (fromException -> Just A) -> True
        _ -> False

  test "`async` propagates (and also returns) async exceptions" do
    ref <- newIORef False
    Ki.scoped_ \scope -> do
      mask \restore -> do
        thread <- Ki.async @_ @() scope (throw B)
        restore (Ki.wait scope) `catch` \case
          Ki.Internal.ThreadFailed (fromException -> Just B) -> writeIORef ref True
          exception -> throwIO exception
        readIORef ref `shouldReturn` True
        Ki.await thread `shouldReturnSuchThat` \case
          Left (fromException -> Just B) -> True
          _ -> False

  test "awaiting a failed `fork`ed thread blocks (sync exception)" do
    Ki.scoped_ \scope -> do
      mask \restore -> do
        thread <- Ki.fork @_ @() scope (throw A)
        restore (Ki.wait scope) `catch` \(Ki.Internal.ThreadFailed _) -> pure ()
        Ki.await thread `shouldThrow` A

  test "awaiting a failed `fork`ed thread blocks (async exception)" do
    Ki.scoped_ \scope -> do
      mask \restore -> do
        thread <- Ki.fork @_ @() scope (throw B)
        restore (Ki.wait scope) `catch` \(Ki.Internal.ThreadFailed _) -> pure ()
        Ki.await thread `shouldThrow` B

  childtest "inherits masking state" \fork -> do
    Ki.scoped_ \scope -> do
      fork scope (getMaskingState `shouldReturn` Unmasked)
      mask_ (fork scope (getMaskingState `shouldReturn` MaskedInterruptible))
      uninterruptibleMask_ (fork scope (getMaskingState `shouldReturn` MaskedUninterruptible))
      Ki.wait scope

  test "provides an unmasking function (`forkWithUnmask`)" do
    Ki.scoped_ \scope -> do
      _thread <- mask_ (Ki.forkWithUnmask scope \unmask -> unmask getMaskingState `shouldReturn` Unmasked)
      Ki.wait scope

  test "provides an unmasking function (`forkWithUnmask_`)" do
    Ki.scoped_ \scope -> do
      mask_ (Ki.forkWithUnmask_ scope \unmask -> unmask getMaskingState `shouldReturn` Unmasked)
      Ki.wait scope

  test "provides an unmasking function (`asyncWithUnmask`)" do
    Ki.scoped_ \scope -> do
      _thread <- (Ki.asyncWithUnmask scope \unmask -> unmask getMaskingState `shouldReturn` Unmasked)
      Ki.wait scope

  test "thread can be awaited after its scope closes" do
    thread <-
      Ki.scoped_ \scope -> do
        thread <- Ki.fork scope (pure ())
        Ki.wait scope
        pure thread
    Ki.await thread `shouldReturn` ()

  test "parent kills children when it throws" do
    ref <- newIORef False
    ignoring @A do
      Ki.scoped_ \scope -> do
        var <- newEmptyMVar
        uninterruptibleMask_ do
          Ki.forkWithUnmask_ scope \unmask -> do
            putMVar var ()
            unmask (threadDelay 1_000_000) `onException` writeIORef ref True
        takeMVar var
        void (throw A)
    readIORef ref `shouldReturn` True

  test "parent kills children when child throws" do
    ref <- newIORef False
    ignoring @A do
      Ki.scoped_ \scope -> do
        uninterruptibleMask_ do
          (Ki.forkWithUnmask_ scope \unmask -> unmask (threadDelay 1_000_000) `onException` writeIORef ref True)
        Ki.fork_ scope (throw A)
        Ki.wait scope
    readIORef ref `shouldReturn` True

  test "`scoped` returns Left Cancelled when it throws its own token" do
    (`shouldReturn` Left Ki.Cancelled) do
      Ki.scoped @_ @() \scope -> do
        Ki.cancel scope
        atomically Ki.cancelledSTM >>= throwIO

  test "`scoped` returns Left Cancelled when it awaits a `fork`ed thread that threw its own token" do
    (`shouldReturn` Left Ki.Cancelled) do
      Ki.scoped @_ @() \scope -> do
        Ki.cancel scope
        Ki.fork scope (atomically Ki.cancelledSTM >>= throwIO) >>= Ki.await

forktest :: String -> (Ki.Context => (Ki.Scope -> (Ki.Context => IO ()) -> IO ()) -> IO ()) -> IO ()
forktest name theTest = do
  test (name ++ " (`fork`)") (theTest \scope action -> void (Ki.fork scope action))
  test (name ++ " (`fork_`)") (theTest Ki.fork_)

childtest :: String -> (Ki.Context => (Ki.Scope -> (Ki.Context => IO ()) -> IO ()) -> IO ()) -> IO ()
childtest name theTest = do
  forktest name theTest
  test (name ++ " (`async`)") (theTest \scope action -> void (Ki.async scope action))

data A = A
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

data B = B
  deriving stock (Eq, Show)

instance Exception B where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException
