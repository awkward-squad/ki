{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Main
  ( main,
  )
where

import Control.Concurrent.Classy hiding (fork, forkWithUnmask, wait)
import Control.Exception
  ( Exception (..),
    asyncExceptionFromException,
    asyncExceptionToException,
  )
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

  test "`scoped` closing while `fork` propagating never deadlocks" (nondeterministic [Right False, Right True]) do
    ref <- newIORef False
    catch
      (scoped \scope -> fork_ scope (throw A))
      (\A -> writeIORef ref True)
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
