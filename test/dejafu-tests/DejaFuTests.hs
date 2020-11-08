{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module DejaFuTests
  ( main,
  )
where

import Control.Concurrent.Classy hiding (fork, forkWithUnmask, wait)
import Control.Exception
  ( Exception (..),
    MaskingState (..),
    SomeException (..),
    asyncExceptionFromException,
    asyncExceptionToException,
    pattern ErrorCall,
  )
import Control.Monad
import Data.Function
import Data.Functor
import Data.Maybe
import DejaFuTestUtils
import Ki.Implicit
import qualified Ki.Internal as Internal
import Prelude

main :: IO ()
main = do
  test "`wait` waits for `fork`" (returns True) do
    ref <- newIORef False
    scoped \scope -> do
      fork_ scope (writeIORef ref True)
      wait scope
    readIORef ref

  test "`wait` waits for `async`" (returns True) do
    ref <- newIORef False
    scoped \scope -> do
      _ <- async scope (writeIORef ref True)
      wait scope
    readIORef ref

  test "`waitFor` sometimes waits for a thread, sometimes kills it" (nondeterministic [Right False, Right True]) do
    ref <- newIORef False
    scoped \scope -> do
      fork_ scope (writeIORef ref True)
      waitFor scope 1
    readIORef ref

  test "using a closed scope throws ErrorCall" (throws (ErrorCall "ki: scope closed")) do
    scope <- scoped pure
    fork_ scope (pure ())

  test "`await` waits" (returns True) do
    scoped \scope -> do
      thread <- async scope (pure ())
      isRight <$> await thread

  test "`await` re-throws synchronous exceptions" (returns True) do
    scoped \scope -> do
      thread <- async scope (throw A)
      isLeft <$> await thread

  test "thread can be awaited after its scope closes" (returns True) do
    thread <- scoped \scope -> do
      thread <- async scope (pure ())
      wait scope
      pure thread
    isRight <$> await thread

  test "`fork` forks a background thread" (returns True) do
    scoped \scope -> do
      var <- newEmptyMVar
      fork_ scope (myThreadId >>= putMVar var)
      (/=) <$> myThreadId <*> takeMVar var

  test "`fork`ed thread inherits masking state" (returns (Unmasked, MaskedInterruptible, MaskedUninterruptible)) do
    scoped \scope -> do
      var1 <- newEmptyMVar
      var2 <- newEmptyMVar
      var3 <- newEmptyMVar
      fork_ scope (getMaskingState >>= putMVar var1)
      mask_ (fork_ scope (getMaskingState >>= putMVar var2))
      uninterruptibleMask_ (fork_ scope (getMaskingState >>= putMVar var3))
      (,,) <$> takeMVar var1 <*> takeMVar var2 <*> takeMVar var3

  test "`fork` propagates synchronous exceptions to parent" (returns (Just A)) do
    catch
      ( scoped \scope -> do
          fork_ scope (throw A)
          wait scope
          pure Nothing
      )
      ( \exception ->
          pure do
            ThreadFailed _threadId exception' <- fromException exception
            fromException exception'
      )

  test "`fork` propagates asynchronous exceptions to parent" (returns (Just B)) do
    catch
      ( scoped \scope -> do
          fork_ scope (throw B)
          wait scope
          pure Nothing
      )
      ( \exception ->
          pure do
            ThreadFailed _threadId exception' <- fromException exception
            fromException exception'
      )

  test "`fork` doesn't propagate `CancelToken`" (returns ()) do
    scoped \scope -> do
      cancelScope scope
      fork_ scope do
        cancelled >>= \case
          Nothing -> throw A
          Just cancelToken -> throw cancelToken
      wait scope

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

  test "`async` doesn't propagate exceptions" (returns ()) (scoped \scope -> void (async scope (throw A)))

  test "`await` returns synchronous exceptions" (returns (Left (Just A))) do
    scoped \scope -> do
      thread <- async scope (throw A)
      await thread <&> \case
        Left (ThreadFailed _threadId exception) -> Left (fromException exception)
        Right () -> Right ()

  test "`await` returns asynchronous exceptions" (returns (Left (Just B))) do
    scoped \scope -> do
      thread <- async scope (throw @_ @_ @() B)
      await thread <&> \case
        Left (ThreadFailed _threadId exception) -> Left (fromException exception)
        Right () -> Right ()

  test "`async` inherits masking state" (returns (Unmasked, MaskedInterruptible, MaskedUninterruptible)) do
    scoped \scope -> do
      thread1 <- async scope getMaskingState
      thread2 <- mask_ (async scope getMaskingState)
      thread3 <- uninterruptibleMask_ (async scope getMaskingState)
      (,,)
        <$> (either throw pure =<< await thread1)
        <*> (either throw pure =<< await thread2)
        <*> (either throw pure =<< await thread3)

  test "`asyncWithUnmask` inherits masking state" (returns (Unmasked, MaskedInterruptible, MaskedUninterruptible)) do
    scoped \scope -> do
      thread1 <- asyncWithUnmask scope \_ -> getMaskingState
      thread2 <- mask_ (asyncWithUnmask scope \_ -> getMaskingState)
      thread3 <- uninterruptibleMask_ (asyncWithUnmask scope \_ -> getMaskingState)
      (,,)
        <$> (either throw pure =<< await thread1)
        <*> (either throw pure =<< await thread2)
        <*> (either throw pure =<< await thread3)

  test "`asyncWithUnmask` provides an unmasking function" (returns Unmasked) do
    scoped \scope -> do
      thread <- mask_ (asyncWithUnmask scope \unmask -> unmask getMaskingState)
      either throw pure =<< await thread

  todo "`forkWithUnmask` inherits masking state"

  todo "`forkWithUnmask` provides an unmasking function"

  todo "`scoped` wraps async exceptions it throws in SyncException"

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

  test "`scoped` kills threads when `fork` throws" (returns True) do
    ref <- newIORef False
    catch
      ( scoped \scope -> do
          uninterruptibleMask_ (forkWithUnmask_ scope \unmask -> unmask block `onException` writeIORef ref True)
          fork_ scope (void (throw A))
          wait scope
          pure False
      )
      ( \exception ->
          case fromException exception of
            Just (ThreadFailed _threadId (fromException -> Just A)) -> readIORef ref
            _ -> pure False
      )

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

await' :: Thread (Either ThreadFailed a) -> P a
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

onException :: P a -> P b -> P a
onException action cleanup =
  catch @_ @SomeException action \ex -> do
    _ <- cleanup
    throw ex
