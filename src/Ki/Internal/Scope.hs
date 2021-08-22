module Ki.Internal.Scope
  ( Scope (..),
    Children (..),
    scopeFork,
    scopeScoped,
    scopeScopedIO,
    scopeWait,
    scopeWaitFor,
    scopeWaitForIO,
    scopeWaitSTM,
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
import Ki.Internal.Duration (Duration)
import Ki.Internal.Prelude
import Ki.Internal.Timeout

-- | A __scope__ delimits the lifetime of all __threads__ created within it.
data Scope = Scope
  { -- | The set of threads that are currently running.
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

scopeFork :: Scope -> ((forall x. IO x -> IO x) -> IO a) -> (Either SomeException a -> IO ()) -> IO ThreadId
scopeFork Scope {scope'runningVar, scope'startingVar} action k =
  uninterruptibleMask \restore -> do
    -- Record the thread as being about to start, and grab an id for it
    childId <-
      atomically do
        starting <- readTVar scope'startingVar
        if starting == -1
          then throwSTM (ErrorCall "ki: scope closed")
          else do
            children <- readTVar scope'runningVar
            let childId = children'nextId children
            writeTVar scope'runningVar $! children {children'nextId = childId + 1}
            writeTVar scope'startingVar $! starting + 1
            pure childId

    childThreadId <-
      forkIO do
        result <- try (action restore)
        -- Perform the internal callback (this is where we decide to propagate the exception and whatnot)
        k result
        -- Delete ourselves from the scope's record of what's running. Why not just IntMap.delete? It might miss (race
        -- condition) - we wouldn't want to delete *nothing*, *then* insert from the parent thread. So just retry until
        -- the parent has recorded us as having started.
        atomically do
          children <- readTVar scope'runningVar
          case IntMap.alterF (maybe Nothing (const (Just Nothing))) childId (children'children children) of
            Nothing -> retry
            Just running -> writeTVar scope'runningVar $! children {children'children = running}

    -- Record the thread as having started
    atomically do
      modifyTVar' scope'startingVar \n -> n -1
      modifyTVar' scope'runningVar \children ->
        children {children'children = IntMap.insert childId childThreadId (children'children children)}

    pure childThreadId

scopeScoped :: MonadUnliftIO m => (Scope -> m a) -> m a
scopeScoped action =
  withRunInIO \unlift -> scopeScopedIO (unlift . action)
{-# SPECIALIZE scopeScoped :: (Scope -> IO a) -> IO a #-}

scopeScopedIO :: (Scope -> IO a) -> IO a
scopeScopedIO f = do
  uninterruptibleMask \restore -> do
    scope'runningVar <- newTVarIO (Children IntMap.empty 0)
    scope'startingVar <- newTVarIO 0
    let scope = Scope {scope'runningVar, scope'startingVar}
    result <- try (restore (f scope))
    children <-
      atomically do
        -- Block until we haven't committed to starting any threads. Without this, we may create a thread concurrently
        -- with closing its scope, and not grab its thread id to throw an exception to.
        blockUntilNoneStarting scope
        -- Write the sentinel value indicating that this scope is closed, and it is an error to try to create a thread
        -- within it.
        writeTVar scope'startingVar (-1)
        -- Remove our parent scope's reference to us. This is necessary for long-lived parent scopes that create and
        -- destroy many child scopes dynamically - we shouldn't leak memory by needlessly growing the parent's list of
        -- children forever. Plus, when/if the parent is cancelled, we'd like as small a list of children as possible to
        -- propagate the cancellation to.
        -- removeContextFromParent
        -- Return the list of currently-running children to kill. Some of them may have *just* started (e.g. if we
        -- initially retried in 'blockUntilNoneStarting' above). That's fine - kill them all!
        readTVar scope'runningVar

    -- Deliver an async exception to every child. While doing so, we may get hit by an async exception ourselves, which
    -- we don't want to just ignore. (Actually, we may have been hit by an arbitrary number of async exceptions,
    -- but it's unclear what we would do with such a list, so we only remember the first one, and ignore the others).
    exceptionReceivedWhileKillingChildren <- killThreads (IntMap.elems (children'children children))

    -- Block until all children have terminated; this relies on children respecting the async exception, which they
    -- must, for correctness. Otherwise, a thread could indeed outlive the scope in which it's created, which is
    -- definitely not structured concurrency!
    atomically (blockUntilNoneRunning scope)

    -- If the callback failed, we don't care if we were thrown an async exception while closing the scope. Otherwise,
    -- throw that exception (if it exists).
    case result of
      Left exception -> throw exception
      Right value -> do
        whenJust exceptionReceivedWhileKillingChildren throw
        pure value
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
blockUntilNoneRunning Scope {scope'runningVar} =
  blockUntilTVar scope'runningVar \(Children children _) -> IntMap.null children

blockUntilNoneStarting :: Scope -> STM ()
blockUntilNoneStarting Scope {scope'startingVar} =
  blockUntilTVar scope'startingVar (== 0)

--------------------------------------------------------------------------------
-- Exception types

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
