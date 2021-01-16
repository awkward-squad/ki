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
    scoped' \_ -> (isJust <$> Ki.cancelled) `shouldReturn` False

  test "`cancel` observable by scope's `cancelled`" do
    scoped' \scope -> do
      Ki.cancel scope
      (isJust <$> Ki.cancelled) `shouldReturn` True

  test "`cancel` observable by inner scope's `cancelled`" do
    scoped' \scope ->
      scoped' \_ -> do
        Ki.cancel scope
        (isJust <$> Ki.cancelled) `shouldReturn` True

  childtest "`cancel` observable by child's `cancelled`" \fork -> do
    ref <- newIORef False
    scoped' \scope -> do
      fork scope do
        Ki.cancel scope
        Ki.cancelled >>= writeIORef ref . isJust
      Ki.wait scope
    readIORef ref `shouldReturn` True

  childtest "`cancel` observable by grandchild's `cancelled`" \fork -> do
    ref <- newIORef False
    scoped' \scope1 -> do
      fork scope1 do
        scoped' \scope2 -> do
          fork scope2 do
            Ki.cancel scope1
            Ki.cancelled >>= writeIORef ref . isJust
          Ki.wait scope2
      Ki.wait scope1
    readIORef ref `shouldReturn` True

  test "inner scope inherits cancellation" do
    scoped' \scope1 -> do
      Ki.cancel scope1
      scoped' \_ -> (isJust <$> Ki.cancelled) `shouldReturn` True

  childtest "child thread inherits cancellation" \fork -> do
    ref <- newIORef False
    scoped' \scope -> do
      Ki.cancel scope
      fork scope (Ki.cancelled >>= writeIORef ref . isJust)
      Ki.wait scope
    readIORef ref `shouldReturn` True

  childtest "creating a child thread throws ErrorCall when the scope is closed" \fork -> do
    scope <- scoped' pure
    fork scope (pure ()) `shouldThrow` ErrorCall "ki: scope closed"

  test "cancelling child context removes parent's ref to it" do
    context0 <- Ki.Internal.newContext
    let count = length <$> readTVarIO (Ki.Internal.context'childrenVar context0)
    context1 <- Ki.Internal.deriveContext context0
    count `shouldReturn` 1
    token <- Ki.Internal.newCancelToken
    atomically (Ki.Internal.cancelContext context1 token)
    count `shouldReturn` 0

  test "closing child scope removes its context's parent's ref to it" do
    scoped' \scope -> do
      let count = length <$> readTVarIO (Ki.Internal.context'childrenVar (Ki.Internal.scope'context scope))
      scoped' \_ -> count `shouldReturn` 1
      count `shouldReturn` 0

  test "`wait` succeeds when no threads are alive" do
    scoped' Ki.wait

  childtest "creates a thread" \fork -> do
    parentThreadId <- myThreadId
    scoped' \scope -> do
      fork scope do
        childThreadId <- myThreadId
        when (parentThreadId == childThreadId) (fail "didn't create a thread")
      Ki.wait scope

  forktest "propagates sync exceptions" \fork ->
    ( scoped' \scope -> do
        fork scope (throwIO A)
        Ki.wait scope
    )
      `shouldThrow` A

  forktest "propagates async exceptions" \fork ->
    ( scoped' \scope -> do
        fork scope (throwIO B)
        Ki.wait scope
    )
      `shouldThrow` B

  forktest "doesn't propagate own CancelToken" \fork ->
    scoped' \scope -> do
      Ki.cancel scope
      fork scope (atomically Ki.cancelledSTM >>= throwIO)
      Ki.wait scope

  forktest "propagates others' CancelTokens" \fork -> do
    ref <- newIORef False
    -- Create an outer scope, just to cancel it.
    scoped' \scope0 -> do
      Ki.cancel scope0
      -- Create an inner scope, which should be cancelled, but by the outer scope's token. So, threads spawned within
      -- scope1 *should* propagate the CancelToken
      catch
        ( scoped' \scope1 -> do
            var <- newEmptyMVar
            fork scope1 do
              takeMVar var -- wait until parent is ready to catch the CancelToken
              atomically Ki.cancelledSTM >>= throwIO
            mask \unmask -> do
              putMVar var ()
              unmask (Ki.wait scope1)
        )
        (\(_ :: Ki.CancelToken) -> writeIORef ref True)
    readIORef ref `shouldReturn` True

  forktest "propagates ScopeClosing if it isn't ours" \fork ->
    ( scoped' \scope -> do
        fork scope (throwIO Ki.Internal.ScopeClosing)
        Ki.wait scope
    )
      `shouldThrow` Ki.Internal.ScopeClosing

  test "`async` returns sync exceptions" do
    scoped' \scope -> do
      result <- Ki.async @_ @() scope (throw A)
      Ki.await result `shouldReturnSuchThat` \case
        Left (fromException -> Just A) -> True
        _ -> False

  test "`async` propagates (and also returns) async exceptions" do
    ref <- newIORef False
    scoped' \scope -> do
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
    scoped' \scope -> do
      mask \restore -> do
        thread <- Ki.fork @_ @() scope (throw A)
        restore (Ki.wait scope) `catch` \(Ki.Internal.ThreadFailed _) -> pure ()
        Ki.await thread `shouldThrow` A

  test "awaiting a failed `fork`ed thread blocks (async exception)" do
    scoped' \scope -> do
      mask \restore -> do
        thread <- Ki.fork @_ @() scope (throw B)
        restore (Ki.wait scope) `catch` \(Ki.Internal.ThreadFailed _) -> pure ()
        Ki.await thread `shouldThrow` B

  childtest "inherits masking state" \fork -> do
    scoped' \scope -> do
      fork scope (getMaskingState `shouldReturn` Unmasked)
      mask_ (fork scope (getMaskingState `shouldReturn` MaskedInterruptible))
      uninterruptibleMask_ (fork scope (getMaskingState `shouldReturn` MaskedUninterruptible))
      Ki.wait scope

  test "provides an unmasking function (`forkWithUnmask`)" do
    scoped' \scope -> do
      _thread <- mask_ (Ki.forkWithUnmask scope \unmask -> unmask getMaskingState `shouldReturn` Unmasked)
      Ki.wait scope

  test "provides an unmasking function (`forkWithUnmask_`)" do
    scoped' \scope -> do
      mask_ (Ki.forkWithUnmask_ scope \unmask -> unmask getMaskingState `shouldReturn` Unmasked)
      Ki.wait scope

  test "provides an unmasking function (`asyncWithUnmask`)" do
    scoped' \scope -> do
      _thread <- (Ki.asyncWithUnmask scope \unmask -> unmask getMaskingState `shouldReturn` Unmasked)
      Ki.wait scope

  test "thread can be awaited after its scope closes" do
    thread <-
      scoped' \scope -> do
        thread <- Ki.fork scope (pure ())
        Ki.wait scope
        pure thread
    Ki.await thread `shouldReturn` ()

  test "parent kills children when it throws" do
    ref <- newIORef False
    ignoring @A do
      scoped' \scope -> do
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
      scoped' \scope -> do
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

scoped' :: Ki.Context => (Ki.Context => Ki.Scope -> IO a) -> IO a
scoped' action =
  Ki.scoped action >>= either throwIO pure

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
