{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Trio.Indef
  ( Context,
    initialize,
    Scope,
    scoped,
    wait,
    waitFor,
    cancelled,
    async,
    asyncMasked,
    Promise,
    await,
    cancel,
    ScopeClosed (..),
    ThreadFailed (..),
  )
where

import Control.Applicative ((<|>))
import Control.Exception (AsyncException (ThreadKilled), Exception (fromException, toException), SomeException, asyncExceptionFromException, asyncExceptionToException)
import Control.Monad (join, unless, void)
import Data.Coerce (coerce)
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.Set (Set)
import qualified Data.Set as Set
import Trio.Internal.Conc (blockUntilTVar, registerBlock, retryingUntilSuccess)
import Trio.Sig (IO, STM, TMVar, TVar, ThreadId, atomically, forkIOWithUnmask, modifyTVar', myThreadId, newEmptyTMVar, newTVar, putTMVar, readTMVar, readTVar, retry, throwIO, throwSTM, throwTo, try, uninterruptibleMask, uninterruptibleMask_, unsafeUnmask, writeTVar)
import Prelude hiding (IO)

-- import Trio.Internal.Debug

-- | A __context__ is used for propagating cancellation signals.
newtype Context
  = Context Scope

-- Implementation note: contexts and scopes are the same thing. We differentiate
-- them in the type system so it's easier to prevent them from getting confused.
--
-- When forking a thread, the scope that it was created in is relevant to it,
-- because we want be able to observe cancellations "deply".
--
-- Consider a call tree like
--
--       foo
--      /   \
--     bar  baz
--            \
--            qux
--
-- If scope 'foo' is cancelled, we want threads running in scope 'qux' to
-- observe it, without an explicit reference to 'foo'. In code:
--
--     foo context =
--       scoped context \scope -> do
--         async scope bar
--         async scope baz
--         waitFor scope 1000 -- We want bar, baz, and qux to notice this
--
--     bar context =
--       atomically (cancelled context) >>= \case
--         False -> do
--           putStrLn "bar: working"
--           threadDelay 1000000
--           bar context
--         True ->
--           putStrLn "bar: cleaning up"
--
--     baz context =
--       scoped context \scope ->
--         async scope qux
--         wait scope
--
--     qux context =
--       atomically (cancelled context) >>= \case
--         False -> do
--           putStrLn "qux: working"
--           threadDelay 1000000
--           qux context
--         True ->
--           putStrLn "qux: cleaning up"
--
-- The necessary relationship is established when creating a new scope with
-- 'scoped':
--
--                when this scope is cancelled
--               /
--     scoped context \scope -> ...
--                       \
--                        this scope is cancelled too
--
--
-- However, if these things both had the same type, it would be easy to
-- accidentally use one in place of the other:
--
--     async scope do
--       -- Oops, encapsulation violation! I just spawned my own sibling!
--       async scope worker
--
--       -- Oops, I just waited on my own scope, which is guaranteed to
--       -- deadlock!
--       atomically (wait scope)
--
--       -- One legitimate use of scope is to check it for cancellation from
--       -- within a thread.
--       atomically (cancelled scope)
--
--       -- The other legitimate use of scope is to derive new scopes, so that
--       -- cancellation of the outer is observed by the inner.
--       scoped scope \scope' ->
--         ...
--
-- So when forking a thread, the scope it's created in is redundantly passed
-- along, wrapped in a newtype, so that it can only make its way back into
-- calls to 'cancelled' and 'scoped'.
--
--     async scope \context -> ...
--                     \
--                      under the hood, this is 'scope'

-- | A __scope__ delimits the lifetime of all __threads__ spawned within it.
--
-- * When a __scope__ is /closed/, all __threads__ are /canceled/.
-- * If a __thread__ throws an exception, the scope is /closed/.
data Scope = Scope
  { -- Invariant: if closed, no threads are starting
    closedVar :: TVar Bool,
    cancelledVar :: TVar CancelledTree,
    runningVar :: TVar (Set ThreadId),
    startingVar :: TVar Int
  }

data CancelledTree
  = Node !Bool ![TVar CancelledTree]

data Promise a = Promise
  { threadId :: ThreadId,
    resultVar :: TMVar (Either SomeException a)
  }

-- | Some actions that use a /closed/ __scope__ throw 'ScopeClosed'.
data ScopeClosed
  = ScopeClosed
  deriving stock (Show)
  deriving anyclass (Exception)

data ThreadFailed = ThreadFailed
  { threadId :: ThreadId,
    exception :: SomeException
  }
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Unexported async variant of 'ThreadFailed'.
data AsyncThreadFailed = AsyncThreadFailed
  { threadId :: ThreadId,
    exception :: SomeException
  }
  deriving stock (Show)

instance Exception AsyncThreadFailed where
  fromException = asyncExceptionFromException
  toException = asyncExceptionToException

translateAsyncThreadFailed :: SomeException -> SomeException
translateAsyncThreadFailed ex =
  case fromException ex of
    Just AsyncThreadFailed {threadId, exception} ->
      toException ThreadFailed {threadId, exception}
    _ -> ex

-- | Initialize a new __context__.
--
-- You should only call this function once.
initialize :: IO Context
initialize =
  coerce (atomically (newScope_ False))

-- | Create a new scope derived from a parent context: the scope inherits
-- whether the context is cancelled, and if it isn't, the context's cancellation
-- tree is adjusted to include the new scope.
newScope :: Context -> STM Scope
newScope (Context Scope {cancelledVar = parentCancelledVar}) = do
  Node c vars <- readTVar parentCancelledVar
  scope@Scope {cancelledVar} <- newScope_ c
  unless c (writeTVar parentCancelledVar $! Node c (cancelledVar : vars))
  pure scope

newScope_ :: Bool -> STM Scope
newScope_ c = do
  cancelledVar <- newTVar "cancelled" (Node c [])
  closedVar <- newTVar "closed" False
  runningVar <- newTVar "running" Set.empty
  startingVar <- newTVar "starting" 0
  pure Scope {cancelledVar, closedVar, runningVar, startingVar}

-- | Perform an action with a new __scope__. When the action finishes, the scope
-- is /closed/.
--
-- /Throws/:
--
--   * 'ThreadFailed' if a __thread__ throws an exception.
--
-- ==== __Examples__
--
-- @
-- 'scoped' context \\scope ->
--   void ('async' scope worker1)
--   void ('async' scope worker2)
--   'wait' scope
-- @
scoped :: Context -> (Scope -> IO a) -> IO a
scoped context f = do
  uninterruptibleMask \restore -> do
    scope <- atomically (newScope context)
    result <- restore (try (f scope))
    closeScope scope
    either (throwIO . translateAsyncThreadFailed) pure result

-- | Block until all __threads__ finish, then close the __scope__.
wait :: Scope -> STM ()
wait Scope {closedVar, runningVar, startingVar} = do
  blockUntilTVar startingVar (== 0)
  blockUntilTVar runningVar Set.null
  writeTVar closedVar True

-- | /Cancel/ a __scope__ and all __scopes__ created within, then block until
-- all __threads__ finish, or the given number of microseconds elapse, after
-- which the __scope__ is /closed/.
waitFor :: Scope -> Int -> IO ()
waitFor scope@(Scope {cancelledVar}) micros
  | micros < 0 = atomically (wait scope)
  | micros == 0 = uninterruptibleMask_ (closeScope scope)
  | otherwise = do
    atomically (cancelTree cancelledVar)
    blockUntilTimeout <- registerBlock micros
    let happyTeardown :: STM (IO ())
        happyTeardown =
          wait scope $> pure ()
    let sadTeardown :: STM (IO ())
        sadTeardown =
          blockUntilTimeout $> uninterruptibleMask_ (closeScope scope)
    (join . atomically) (happyTeardown <|> sadTeardown)
  where
    cancelTree :: TVar CancelledTree -> STM ()
    cancelTree treeVar = do
      Node c trees <- readTVar treeVar
      unless c do
        writeTVar treeVar (Node True trees)
        for_ trees cancelTree

-- Precondition: uninterruptibly masked
closeScope :: Scope -> IO ()
closeScope Scope {closedVar, runningVar, startingVar} = do
  (join . atomically) do
    readTVar closedVar >>= \case
      False -> do
        blockUntilTVar startingVar (== 0)
        writeTVar closedVar True
        pure do
          cancellingThreadId <- myThreadId
          children <- atomically (readTVar runningVar)
          for_ (Set.delete cancellingThreadId children) \child ->
            -- Kill the child with asynchronous exceptions unmasked, because
            -- we don't want to deadlock with a child concurrently trying to
            -- throw a exception back to us. But if any exceptions are thrown
            -- to us during this time, just ignore them. We already have an
            -- exception to throw, and we prefer it because it was delivered
            -- first.
            retryingUntilSuccess (unsafeUnmask (throwTo child ThreadKilled))

          if Set.member cancellingThreadId children
            then do
              atomically (blockUntilTVar runningVar ((== 1) . Set.size))
              throwIO ThreadKilled
            else atomically (blockUntilTVar runningVar Set.null)
      True -> pure (pure ())

-- | Return whether the __context__ is /canceled/.
cancelled :: Context -> STM Bool
cancelled (Context Scope {cancelledVar}) = do
  Node c _ <- readTVar cancelledVar
  pure c

-- | Fork a __thread__. The derived __context__ should shadow any other
-- __contexts__ in scope.
--
-- /Throws/:
--
--   * 'ScopeClosed' if the __scope__ is closed.
async :: Scope -> (Context -> IO a) -> IO (Promise a)
async scope action =
  asyncMasked scope \context unmask -> unmask (action context)

-- | Like 'async', but forks a __thread__ with asynchronous exceptions
-- uninterruptibly masked.
--
-- /Throws/:
--
--   * 'ScopeClosed' if the __scope__ is closed.
asyncMasked ::
  Scope ->
  (Context -> (forall x. IO x -> IO x) -> IO a) ->
  IO (Promise a)
asyncMasked scope@(Scope {closedVar, runningVar, startingVar}) action = do
  uninterruptibleMask_ do
    atomically do
      readTVar closedVar >>= \case
        False -> modifyTVar' startingVar (+ 1)
        True -> throwSTM ScopeClosed

    parentThreadId <- myThreadId
    resultVar <- atomically (newEmptyTMVar "result")

    childThreadId <-
      forkIOWithUnmask \unmask -> do
        childThreadId <- myThreadId
        result <- try (action (Context scope) unmask)
        case result of
          Left (NotThreadKilled exception) ->
            throwTo parentThreadId (AsyncThreadFailed childThreadId exception)
          _ -> pure ()
        atomically do
          running <- readTVar runningVar
          if Set.member childThreadId running
            then do
              putTMVar resultVar result
              writeTVar runningVar $! Set.delete childThreadId running
            else retry

    atomically do
      modifyTVar' startingVar (subtract 1)
      modifyTVar' runningVar (Set.insert childThreadId)

    pure
      Promise
        { threadId = childThreadId,
          resultVar
        }

-- | Wait for a __promise__ to be fulfilled.
--
-- /Throws/:
--
--   * 'ThreadFailed' if the __thread__ failed.
await :: Promise a -> STM a
await Promise {resultVar, threadId} =
  readTMVar resultVar >>= \case
    Left exception -> throwSTM ThreadFailed {threadId, exception}
    Right result -> pure result

-- | Kill a __thread__ and wait for it to finish.
--
-- /Throws/:
--
--   * 'ThreadKilled' if a __thread__ attempts to cancel its own __promise__.
cancel :: Promise a -> IO ()
cancel Promise {resultVar, threadId} = do
  throwTo threadId ThreadKilled
  void (atomically (readTMVar resultVar))

pattern NotThreadKilled :: SomeException -> SomeException
pattern NotThreadKilled ex <-
  (asNotThreadKilled -> Just ex)

asNotThreadKilled :: SomeException -> Maybe SomeException
asNotThreadKilled ex
  | Just ThreadKilled <- fromException ex = Nothing
  | otherwise = Just ex
