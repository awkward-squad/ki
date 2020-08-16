module Ki.Fork
  ( fork,
    forkWithUnmask,
  )
where

import Control.Exception (AsyncException (ThreadKilled), Exception (fromException))
import Ki.AsyncThreadFailed (AsyncThreadFailed (..))
import Ki.Context (Context)
import qualified Ki.Context
import Ki.Prelude
import Ki.Scope (Scope)
import qualified Ki.Scope

-- | Variant of 'async' that does not return a handle to the __thread__.
--
-- If the __thread__ throws an exception, the exception is propagated up the call tree to the __thread__ that opened its
-- __scope__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
fork :: Scope -> IO () -> IO ()
fork scope action =
  forkWithRestore scope \restore -> restore action

-- | Variant of 'fork' that provides the __thread__ a function that unmasks asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
forkWithUnmask :: Scope -> ((forall x. IO x -> IO x) -> IO ()) -> IO ()
forkWithUnmask scope action =
  forkWithRestore scope \restore -> restore (action unsafeUnmask)

forkWithRestore :: Scope -> ((forall x. IO x -> IO x) -> IO ()) -> IO ()
forkWithRestore scope action = do
  parentThreadId <- myThreadId
  _ <-
    Ki.Scope.fork scope action \result ->
      whenLeft result \exception ->
        whenM
          (shouldPropagateException (Ki.Scope.context scope) exception)
          (throwTo parentThreadId (AsyncThreadFailed exception))
  pure ()

shouldPropagateException :: Context -> SomeException -> IO Bool
shouldPropagateException context exception =
  case fromException exception of
    Just ThreadKilled -> pure False
    Just _ -> pure True
    Nothing ->
      case fromException exception of
        Just (Ki.Context.Cancelled token) -> atomically ((/= token) <$> Ki.Context.cancelled context <|> pure True)
        Nothing -> pure True
