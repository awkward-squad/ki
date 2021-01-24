{-# LANGUAGE PatternSynonyms #-}

module Ki.Internal.Scope
  ( Scope (..),
    Children (..),
    scopeCancel,
    scopeCancelIO,
    scopeCancelledSTM,
    scopeFork,
    scopeOwnsCancelTokenSTM,
    scopeScoped,
    scopeScopedIO,
    scopeWait,
    scopeWaitFor,
    scopeWaitForIO,
    scopeWaitSTM,
    Cancelled (..),
    ScopeClosing (..),
    ThreadFailed (..),
  )
where

import Control.Exception
  ( Exception (fromException, toException),
    asyncExceptionFromException,
    asyncExceptionToException,
    pattern ErrorCall,
  )
import Control.Monad.IO.Unlift (MonadUnliftIO (withRunInIO))
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Monoid as Monoid
import Ki.Internal.CancelToken
import Ki.Internal.Context
import Ki.Internal.Duration (Duration)
import Ki.Internal.Prelude
import Ki.Internal.Timeout

-- | A __scope__ delimits the lifetime of all __threads__ created within it.
data Scope = Scope
  { scope'context :: {-# UNPACK #-} !Context,
    -- | The set of threads that are currently running.
    scope'runningVar :: {-# UNPACK #-} !(TVar Children),
    -- | The number of threads that are *guaranteed* to be about to start, in the sense that only the GHC scheduler can
    -- continue to delay; no async exception can strike here and prevent one of these threads from starting.
    --
    -- If this number is non-zero, and that's problematic (e.g. because we're trying to cancel this scope), we always
    -- respect it and wait for it to drop to zero before proceeding.
    --
    -- Sentinel value: -1 means the scope is closed.
    scope'startingVar :: {-# UNPACK #-} !(TVar Int)
  }

-- | Children, keyed by a monotonically increasing integer, along with the next integer to use. This allows us to kill
-- child threads in the order they were created.
--
-- It would probably be too slow to throw an async exception to a child, then synchronously wait for the child to
-- terminate before throwing an async exception to the next child, so instead we just deliver the exceptions in order,
-- which is still a useful property to have.
data Children = Children
  { children'children :: !(IntMap ThreadId),
    children'nextId :: {-# UNPACK #-} !Int
  }

scopeCancel :: MonadIO m => Scope -> m ()
scopeCancel =
  liftIO . scopeCancelIO
{-# SPECIALIZE scopeCancel :: Scope -> IO () #-}

scopeCancelIO :: Scope -> IO ()
scopeCancelIO scope = do
  token <- newCancelToken
  atomically (cancelContext (scope'context scope) token)

scopeCancelledSTM :: Scope -> STM (IO a)
scopeCancelledSTM scope =
  throwIO <$> contextCancelToken (scope'context scope)

scopeCancelStateSTM :: Scope -> STM CancelState
scopeCancelStateSTM =
  readTVar . context'cancelStateVar . scope'context

-- | Close a scope, kill all of the running threads, and return the first async exception delivered to us while doing
-- so, if any.
--
-- Preconditions:
--   * The set of threads doesn't include us
--   * We're uninterruptibly masked
closeScope :: STM () -> Scope -> IO (Maybe SomeException)
closeScope removeContextFromParent scope = do
  children <-
    atomically do
      -- Retry until we haven't committed to starting any threads. Without this, we may create a thread concurrently
      -- with closing its scope, and not grab its thread id to throw an exception to.
      blockUntilNoneStarting scope
      -- Write the sentinel value indicating that this scope is closed, and it is an error to try to create a thread
      -- within it.
      writeTVar (scope'startingVar scope) $! (-1)
      -- Remove our parent scope's reference to us. This is necessary for long-lived parent scopes that create and
      -- destroy many child scopes dynamically - we shouldn't leak memory by needlessly growing the parent's list of
      -- children forever. Plus, when/if the parent is cancelled, we'd like as small a list of children as possible to
      -- propagate the cancellation to.
      removeContextFromParent
      -- Return the list of currently-running children to kill. Some of them may have *just* started (e.g. if we
      -- initially retried in 'blockUntilNoneStarting' above). That's fine - kill them all!
      readTVar (scope'runningVar scope)
  -- Deliver an async exception to every child. While doing so, we may have been hit by an async exception ourselves,
  -- which we don't want to just ignore. (Actually, we may have been hit by an arbitrary number of async exceptions, but
  -- it's unclear what we would do with such a list, so we only remember the first one, and ignore the others).
  exception <- killThreads (IntMap.elems (children'children children))
  -- Block until all children have terminated; this relies on children respecting the async exception, which they must,
  -- for correctness. Otherwise, a thread could indeed outlive the scope in which it's created, which is definitely not
  -- what you might call structured concurrency!
  atomically (blockUntilNoneRunning scope)
  pure exception

scopeFork :: Scope -> ((forall x. IO x -> IO x) -> IO a) -> (Either SomeException a -> IO ()) -> IO ThreadId
scopeFork scope action k =
  uninterruptibleMask \restore -> do
    -- Record the thread as being about to start, and grab an id for it
    childId <-
      atomically do
        starting <- readTVar (scope'startingVar scope)
        if starting == -1
          then throwSTM (ErrorCall "ki: scope closed")
          else do
            children <- readTVar (scope'runningVar scope)
            let childId = children'nextId children
            writeTVar (scope'runningVar scope) $! children {children'nextId = childId + 1}
            writeTVar (scope'startingVar scope) $! starting + 1
            pure childId

    -- Fork the thread
    childThreadId <-
      forkIO do
        -- Perform the user-provided action
        result <- try (action restore)
        -- Perform the internal callback (this is where we decide to propagate the exception and whatnot)
        k result
        -- Delete ourselves from the scope's record of what's running. Why not just IntMap.delete? It might miss (race
        -- condition) - we wouldn't want to delete *nothing*, *then* insert from the parent thread. So just retry until
        -- the parent has recorded us as having started.
        atomically do
          children <- readTVar (scope'runningVar scope)
          case IntMap.alterF (maybe Nothing (const (Just Nothing))) childId (children'children children) of
            Nothing -> retry
            Just running -> writeTVar (scope'runningVar scope) $! children {children'children = running}

    -- Record the thread as having started
    atomically do
      modifyTVar' (scope'startingVar scope) \n -> n -1
      modifyTVar' (scope'runningVar scope) \children ->
        children {children'children = IntMap.insert childId childThreadId (children'children children)}

    pure childThreadId

scopeOwnsCancelTokenSTM :: Scope -> CancelToken -> STM Bool
scopeOwnsCancelTokenSTM scope cancelToken =
  scopeCancelStateSTM scope <&> \case
    CancelState'NotCancelled -> False
    CancelState'Cancelled ourToken -> cancelToken == ourToken

scopeScoped :: MonadUnliftIO m => Context -> (Scope -> m a) -> m (Either Cancelled a)
scopeScoped context action =
  withRunInIO \unlift -> scopeScopedIO context (unlift . action)
{-# SPECIALIZE scopeScoped :: Context -> (Scope -> IO a) -> IO (Either Cancelled a) #-}

scopeScopedIO :: Context -> (Scope -> IO a) -> IO (Either Cancelled a)
scopeScopedIO parentContext f = do
  (scope'context, removeContextFromParent) <- deriveContext parentContext
  scope'runningVar <- newTVarIO (Children IntMap.empty 0)
  scope'startingVar <- newTVarIO 0
  let scope :: Scope
      scope =
        Scope {scope'context, scope'runningVar, scope'startingVar}
  uninterruptibleMask \restore -> do
    result <- try (restore (f scope))
    closeScopeException <- closeScope removeContextFromParent scope
    -- If the callback failed, we don't care if we were thrown an async exception while closing the scope.
    -- Otherwise, throw that exception (if it exists).
    case result of
      Left exception ->
        case fromException exception of
          Just thrownToken ->
            atomically (scopeOwnsCancelTokenSTM scope thrownToken) >>= \case
              False -> throw exception
              True -> pure (Left Cancelled)
          _ -> throw exception
      Right value -> do
        whenJust closeScopeException throw
        pure (Right value)
  where
    -- If applicable, unwrap the 'ThreadFailed' (assumed to have come from one of our children).
    throw :: SomeException -> IO a
    throw exception =
      case fromException exception of
        Just (ThreadFailed threadFailedException) -> throwIO threadFailedException
        Nothing -> throwIO exception

scopeWait :: MonadIO m => Scope -> m ()
scopeWait =
  liftIO . scopeWaitIO
{-# SPECIALIZE scopeWait :: Scope -> IO () #-}

scopeWaitIO :: Scope -> IO ()
scopeWaitIO =
  atomically . scopeWaitSTM

scopeWaitFor :: MonadIO m => Scope -> Duration -> m ()
scopeWaitFor scope duration =
  liftIO (scopeWaitForIO scope duration)
{-# SPECIALIZE scopeWaitFor :: Scope -> Duration -> IO () #-}

scopeWaitForIO :: Scope -> Duration -> IO ()
scopeWaitForIO scope duration =
  timeoutSTM duration (pure <$> scopeWaitSTM scope) (pure ())

scopeWaitSTM :: Scope -> STM ()
scopeWaitSTM scope = do
  blockUntilNoneRunning scope
  blockUntilNoneStarting scope

--------------------------------------------------------------------------------
-- Scope helpers

blockUntilNoneRunning :: Scope -> STM ()
blockUntilNoneRunning scope =
  blockUntilTVar (scope'runningVar scope) \(Children children _) -> IntMap.null children

blockUntilNoneStarting :: Scope -> STM ()
blockUntilNoneStarting scope =
  blockUntilTVar (scope'startingVar scope) (== 0)

--------------------------------------------------------------------------------
-- Exception types

data Cancelled = Cancelled
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

-- | Exception thrown by a parent __thread__ to its children when its __scope__ is closing.
data ScopeClosing
  = ScopeClosing
  deriving stock (Eq, Show)

instance Exception ScopeClosing where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

-- | Exception thrown by a child __thread__ to its parent, if it fails unexpectedly.
newtype ThreadFailed
  = ThreadFailed SomeException
  deriving stock (Show)

instance Exception ThreadFailed where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

--------------------------------------------------------------------------------
-- Misc. utils

killThreads :: [ThreadId] -> IO (Maybe SomeException)
killThreads =
  (`fix` mempty) \loop !acc -> \case
    [] -> pure (Monoid.getFirst acc)
    threadId : threadIds ->
      -- We unmask because we don't want to deadlock with a thread
      -- that is concurrently trying to throw an exception to us with
      -- exceptions masked.
      try (unsafeUnmask (throwTo threadId ScopeClosing)) >>= \case
        -- don't drop thread we didn't (necessarily) deliver the exception to
        Left exception -> loop (acc <> Monoid.First (Just exception)) (threadId : threadIds)
        Right () -> loop acc threadIds

blockUntilTVar :: TVar a -> (a -> Bool) -> STM ()
blockUntilTVar var f = do
  value <- readTVar var
  unless (f value) retry
