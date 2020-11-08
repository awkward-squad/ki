module Main (main) where

import Control.Concurrent.STM
import Data.Functor
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
    Ki.scoped \_ ->
      (isJust <$> Ki.cancelled) `shouldReturn` False

  test "`cancelScope` observable by scope's `cancelled`" do
    Ki.scoped \scope -> do
      Ki.cancelScope scope
      (isJust <$> Ki.cancelled) `shouldReturn` True

  test "`cancelScope` observable by inner scope's `cancelled`" do
    Ki.scoped \scope1 ->
      Ki.scoped \_ -> do
        Ki.cancelScope scope1
        (isJust <$> Ki.cancelled) `shouldReturn` True

  test "`cancelScope` observable by child's `cancelled`" do
    Ki.scoped \scope1 -> do
      thread <-
        Ki.fork scope1 do
          Ki.cancelScope scope1
          isJust <$> Ki.cancelled
      Ki.await thread `shouldReturn` True

  test "`cancelScope` observable by grandchild's `cancelled`" do
    Ki.scoped \scope1 -> do
      thread1 <-
        Ki.fork scope1 do
          Ki.scoped \scope2 -> do
            thread2 <-
              Ki.fork scope2 do
                Ki.cancelScope scope1
                isJust <$> Ki.cancelled
            Ki.await thread2
      Ki.await thread1 `shouldReturn` True

  test "inner scope inherits cancellation" do
    Ki.scoped \scope1 -> do
      Ki.cancelScope scope1
      Ki.scoped \_ -> (isJust <$> Ki.cancelled) `shouldReturn` True

  test "child thread inherits cancellation" do
    Ki.scoped \scope -> do
      Ki.cancelScope scope
      (Ki.await =<< Ki.fork scope (isJust <$> Ki.cancelled)) `shouldReturn` True

  test "cancelled child context removes parent's ref to it" do
    ctx0 <- atomically Ki.Internal.newCtxSTM
    ctx1 <- atomically (Ki.Internal.deriveCtx ctx0)
    (length <$> readTVarIO (Ki.Internal.childrenVar ctx0)) `shouldReturn` 1
    Ki.Internal.cancelCtx ctx1
    (length <$> readTVarIO (Ki.Internal.childrenVar ctx0)) `shouldReturn` 0

  test "`wait` succeeds when no threads are alive" do
    Ki.scoped Ki.wait
