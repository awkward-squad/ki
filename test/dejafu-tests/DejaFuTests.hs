{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Main
  ( main,
  )
where

import Control.Concurrent.Classy hiding (fork, forkWithUnmask, wait)
import Control.Exception
  ( Exception (..),
    SomeException (..),
    asyncExceptionFromException,
    asyncExceptionToException,
  )
import Control.Monad
import Data.Maybe
import DejaFuTestUtils
import Ki.Implicit
import Prelude

main :: IO ()
main = do
  test "`waitFor` waits for a duration" (nondeterministic [Right False, Right True]) do
    ref <- newIORef False
    scoped \scope -> do
      fork_ scope (writeIORef ref True)
      waitFor scope 1
    readIORef ref

  {- seems like a dejafu bug
  test "`fork` propagates async exceptions to parent" (throws ThreadKilled) do
    scoped \scope -> do
      var <- newEmptyMVar
      fork_ scope do
        myThreadId >>= putMVar var
        block
      takeMVar var >>= killThread
      block
  -}

  -- TODO move to unit test
  test "`scoped` kills threads when it throws" (returns True) do
    ref <- newIORef False
    ignoring @A do
      scoped \scope -> do
        var <- newEmptyMVar
        uninterruptibleMask_ do
          forkWithUnmask_ scope \unmask -> do
            putMVar var ()
            unmask block `onException` writeIORef ref True
        takeMVar var
        void (throw A)
    readIORef ref

  -- TODO move to unit test
  test "`scoped` kills threads when `fork` throws" (returns True) do
    ref <- newIORef False
    catch
      ( scoped \scope -> do
          uninterruptibleMask_ (forkWithUnmask_ scope \unmask -> unmask block `onException` writeIORef ref True)
          fork_ scope (throw A)
          wait scope
          pure False
      )
      ( \exception ->
          case fromException exception of
            Just (ThreadFailed _threadId (fromException -> Just A)) -> readIORef ref
            _ -> pure False
      )

  test "`scoped` closing while `fork` propagating never deadlocks" (nondeterministic [Right False, Right True]) do
    ref <- newIORef False
    catch
      (scoped \scope -> fork_ scope (throw A))
      (\(ThreadFailed _threadId _exception) -> writeIORef ref True)
    readIORef ref

  test "thread waiting on its own scope deadlocks" deadlocks do
    scoped \scope -> do
      fork_ scope (wait scope)
      wait scope

  test "thread waiting on its own scope allows async exceptions" (returns ()) do
    scoped \scope -> fork_ scope (wait scope)

data A
  = A
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

data B
  = B
  deriving stock (Eq, Show)

instance Exception B where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

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
