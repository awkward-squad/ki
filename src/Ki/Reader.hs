-- | This module exposes an API that uses a reader monad to pass around the __context__ implicitly. If you do not intend
-- to use soft-cancellation, you may want to use the simpler API exposed by "Ki".
--
-- For an example of how to integrate this library with your reader monad, click @Example@ on the left or scroll down to
-- the bottom of this module.
module Ki.Reader
  ( -- * Context
    Context,
    globalContext,
    HasContext (..),

    -- * Scope
    Scope,
    scoped,
    scoped_,
    Ki.wait,
    waitSTM,
    waitFor,

    -- * Creating threads
    Thread,

    -- ** Fork
    fork,
    fork_,
    forkWithUnmask,
    forkWithUnmask_,

    -- ** Async
    async,
    asyncWithUnmask,

    -- ** Await
    Ki.await,
    awaitSTM,
    awaitFor,

    -- * Soft-cancellation
    CancelToken,
    Ki.Implicit.cancel,
    cancelled,
    cancelledSTM,
    Cancelled (Cancelled),

    -- * Miscellaneous
    Duration,
    microseconds,
    milliseconds,
    seconds,
    timeoutSTM,
    sleep,

    -- * Example
    -- $example
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Ki
import qualified Ki.Implicit
import Ki.Internal.CancelToken (CancelToken)
import Ki.Internal.Context (Context, contextCancelToken, globalContext)
import Ki.Internal.Duration (Duration, microseconds, milliseconds, seconds)
import Ki.Internal.Prelude
import Ki.Internal.Scope (Cancelled (Cancelled), Scope (scope'context), scopeScoped, scopeWaitSTM)
import Ki.Internal.Thread
  ( Thread (thread'Await),
    threadAsync,
    threadAsyncWithUnmask,
    threadAwaitFor,
    threadFork,
    threadForkWithUnmask,
    threadForkWithUnmask_,
    threadFork_,
  )
import Ki.Internal.Timeout (timeoutSTM)

-- $example
--
-- You may have an application monad that is defined similar to the following.
--
-- @
-- data Env
--   = Env
--   { ...
--   }
--
-- newtype App a
--   = App { runApp :: Env -> IO a }
--
-- instance MonadUnliftIO App where ...
-- @
--
-- To use this module, first add one field to your @Env@ type that holds a __context__.
--
-- @
-- data Env
--   = Env
--   { ...
--   , envContext :: 'Ki.Reader.Context'
--   , ...
--   }
-- @
--
-- Then, write a 'Ki.Reader.HasContext' instance, which is a bit of boilerplate that encapsulates how to get and set
-- this field.
--
-- @
-- instance 'Ki.Reader.HasContext' App where
--   'Ki.Reader.askContext' =
--     App \\env -> pure (envContext env)
--
--   'Ki.Reader.withContext' context action =
--     App \\env -> runApp action env{ envContext = context }
-- @
--
-- And finally, when running your monad down to @IO@ in @main@ by providing an initial environment, use
-- 'Ki.Reader.globalContext'.
--
-- @
-- main :: IO ()
-- main =
--   runApp initialEnv action
--
-- initialEnv :: Env
-- initialEnv =
--   Env
--     { ...
--     , envContext = 'Ki.Reader.globalContext'
--     , ...
--     }
--
-- action :: App ()
-- action =
--   ...
-- @

-- | The class of reader monads that contain a __context__ in their environment.
class MonadUnliftIO m => HasContext m where
  -- | Project the __context__ from the environment.
  askContext :: m Context

  -- | Run an @m@ action, replacing its __context__ with the one provided.
  withContext :: Context -> m a -> m a

-- | Create a __thread__ within a __scope__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
async ::
  HasContext m =>
  -- |
  Scope ->
  -- |
  m a ->
  m (Thread (Either SomeException a))
async scope action =
  threadAsync scope (with scope action)

-- | Variant of 'Ki.Reader.async' that provides the __thread__ a function that unmasks asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
asyncWithUnmask ::
  HasContext m =>
  -- |
  Scope ->
  -- |
  ((forall x. m x -> m x) -> m a) ->
  m (Thread (Either SomeException a))
asyncWithUnmask scope action =
  threadAsyncWithUnmask scope \unmask -> with scope (action unmask)

-- | Variant of 'Ki.Reader.await' that gives up after the given duration.
awaitFor ::
  MonadIO m =>
  -- |
  Thread a ->
  -- |
  Duration ->
  m (Maybe a)
awaitFor =
  threadAwaitFor
{-# INLINE awaitFor #-}

-- | @STM@ variant of 'Ki.Reader.await'.
awaitSTM ::
  -- |
  Thread a ->
  STM a
awaitSTM =
  thread'Await

-- | Return whether the current __context__ is /cancelled/.
--
-- __Threads__ running in a /cancelled/ __context__ should terminate as soon as possible. The __cancel token__ may be
-- thrown to fulfill the /cancellation/ request in case the __thread__ is unable or unwilling to terminate normally with
-- a value.
cancelled ::
  HasContext m =>
  -- |
  m (Maybe CancelToken)
cancelled =
  either Just (const Nothing) <$> cancelledSTM (pure ())

-- | @STM@ variant of 'Ki.Reader.cancelled'.
cancelledSTM ::
  HasContext m =>
  -- |
  STM a ->
  m (Either CancelToken a)
cancelledSTM action = do
  context <- askContext
  liftIO (atomically (Left <$> contextCancelToken context <|> Right <$> action))

-- | Create a child __thread__ within a __scope__.
--
-- If the child throws an exception, the exception is immediately propagated to its parent, unless the exception is a
-- __cancel token__ that originated from its parent's __scope__ being /cancelled/.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
fork ::
  HasContext m =>
  -- |
  Scope ->
  -- |
  m a ->
  m (Thread a)
fork scope action =
  threadFork scope (with scope action)

-- | Variant of 'Ki.Reader.fork' that does not return a handle to the child __thread__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
fork_ ::
  HasContext m =>
  -- |
  Scope ->
  -- |
  m () ->
  m ()
fork_ scope action =
  threadFork_ scope (with scope action)

-- | Variant of 'Ki.Reader.fork' that provides the child __thread__ a function that unmasks asynchronous exceptions.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
forkWithUnmask ::
  HasContext m =>
  -- |
  Scope ->
  -- |
  ((forall x. m x -> m x) -> m a) ->
  m (Thread a)
forkWithUnmask scope action =
  threadForkWithUnmask scope \unmask -> with scope (action unmask)

-- | Variant of 'Ki.Reader.forkWithUnmask' that does not return a handle to the child __thread__.
--
-- /Throws/:
--
--   * Calls 'error' if the __scope__ is /closed/.
forkWithUnmask_ ::
  HasContext m =>
  -- |
  Scope ->
  -- |
  ((forall x. m x -> m x) -> m ()) ->
  m ()
forkWithUnmask_ scope action =
  threadForkWithUnmask_ scope \unmask -> with scope (action unmask)

-- | Open a __scope__, perform an action with it, then close the __scope__.
--
-- When the __scope__ is closed, all remaining __threads__ created within it are killed.
--
-- The __scope__'s __context__ may become /cancelled/; if it does, and the provided action fulfills the __cancellation__
-- request by throwing the corresponding __cancel token__, this function will return 'Ki.Reader.Cancelled'.
--
-- ==== __Examples__
--
-- @
-- 'Ki.Reader.scoped' \\scope -> do
--   'Ki.Reader.fork_' scope worker1
--   'Ki.Reader.fork_' scope worker2
--   'Ki.Reader.wait' scope
-- @
scoped ::
  HasContext m =>
  -- |
  (Scope -> m a) ->
  -- |
  m (Either Cancelled a)
scoped action = do
  context <- askContext
  scopeScoped context \scope -> with scope (action scope)

-- | Variant of 'Ki.Reader.scoped' that throws 'Ki.Reader.Cancelled' instead of returning it.
scoped_ ::
  HasContext m =>
  -- |
  (Scope -> m a) ->
  m a
scoped_ action =
  scoped action >>= \case
    Left cancelled_ -> liftIO (throwIO cancelled_)
    Right value -> pure value
{-# INLINE scoped_ #-}

-- | __Context__-aware, duration-based @threadDelay@.
--
-- /Throws/:
--
--   * Throws 'Ki.CancelToken.CancelToken' if the current __context__ is (or becomes) /cancelled/.
sleep ::
  HasContext m =>
  -- |
  Duration ->
  m ()
sleep duration = do
  context <- askContext
  timeoutSTM duration (liftIO . throwIO <$> contextCancelToken context) (pure ())

-- | Variant of 'Ki.Reader.wait' that waits for up to the given duration. This is useful for giving __threads__ some
-- time to fulfill a /cancellation/ request before killing them.
waitFor ::
  MonadIO m =>
  -- |
  Scope ->
  -- |
  Duration ->
  m ()
waitFor =
  Ki.waitFor
{-# INLINE waitFor #-}

-- | @STM@ variant of 'Ki.Reader.wait'.
waitSTM ::
  -- |
  Scope ->
  STM ()
waitSTM =
  scopeWaitSTM

--

with :: HasContext m => Scope -> m a -> m a
with scope =
  withContext (scope'context scope)
