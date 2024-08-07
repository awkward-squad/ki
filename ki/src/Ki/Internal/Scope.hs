module Ki.Internal.Scope
  ( Scope,
    scoped,
    awaitAll,
    fork,
    forkWith,
    forkWith_,
    fork_,
    forkTry,
    forkTryWith,
  )
where

import Control.Concurrent (ThreadId, myThreadId, throwTo)
import Control.Concurrent.MVar (MVar, newEmptyMVar, tryPutMVar, tryTakeMVar)
import Control.Exception
  ( Exception (fromException, toException),
    MaskingState (..),
    SomeAsyncException,
    SomeException,
    asyncExceptionFromException,
    asyncExceptionToException,
    throwIO,
    try,
    uninterruptibleMask,
    pattern ErrorCall,
  )
import Control.Monad (guard, when)
import Data.Foldable (for_)
import Data.Functor (void)
import Data.IntMap (IntMap)
import qualified Data.IntMap.Lazy as IntMap.Lazy
import Data.Void (Void, absurd)
import GHC.Conc
  ( STM,
    TVar,
    atomically,
    enableAllocationLimit,
    labelThread,
    newTVarIO,
    readTVar,
    retry,
    setAllocationCounter,
    throwSTM,
    writeTVar,
  )
import GHC.Conc.Sync (readTVarIO)
import GHC.IO (unsafeUnmask)
import IntSupply (IntSupply)
import qualified IntSupply
import Ki.Internal.ByteCount (byteCountToInt64)
import Ki.Internal.IO
  ( IOResult (..),
    UnexceptionalIO (..),
    assertM,
    exceptionIs,
    interruptiblyMasked,
    unexceptionalTry,
    unexceptionalTryEither,
    uninterruptiblyMasked,
  )
import Ki.Internal.NonblockingSTM
import Ki.Internal.Propagating (Tid, peelOffPropagating, propagate)
import Ki.Internal.Thread (Thread, makeThread)
import Ki.Internal.ThreadAffinity (forkWithAffinity)
import Ki.Internal.ThreadOptions (ThreadOptions (..), defaultThreadOptions)

-- | A scope.
--
-- ==== __👉 Details__
--
-- * A scope delimits the lifetime of all threads created within it.
--
-- * A scope is only valid during the callback provided to 'Ki.scoped'.
--
-- * The thread that creates a scope is considered the parent of all threads created within it.
--
-- * All threads created within a scope can be awaited together (see 'Ki.awaitAll').
--
-- * All threads created within a scope are terminated when the scope closes.
data Scope = Scope
  { -- The MVar that a child tries to put to, in the case that it tries to propagate an exception to its parent, but
    -- gets delivered an exception from its parent concurrently (which interrupts the throw). The parent must raise
    -- exceptions in its children with asynchronous exceptions uninterruptibly masked for correctness, yet we don't want
    -- a parent in the process of tearing down to miss/ignore this exception that we're trying to propagate?
    --
    -- Why a single-celled MVar? What if two siblings are fighting to inform their parent of their death? Well, only
    -- one exception can be propagated by the parent anyway, so we wouldn't need or want both.
    childExceptionVar :: {-# UNPACK #-} !(MVar SomeException),
    -- The set of child threads that are currently running, each keyed by a monotonically increasing int.
    childrenVar :: {-# UNPACK #-} !(TVar (IntMap ThreadId)),
    -- The supply that holds the (int) key to use for the next child thread.
    nextChildIdSupply :: {-# UNPACK #-} !IntSupply,
    -- The id of the thread that created the scope, which is considered the parent of all threads created within it.
    parentThreadId :: {-# UNPACK #-} !ThreadId,
    statusVar :: {-# UNPACK #-} !(TVar ScopeStatus)
  }

-- The scope status: either open (allowing new threads to be created), closing (disallowing new threads to be
-- created, and in the process of killing running children), or closed (at the very end of `scoped`)
type ScopeStatus = Int

-- The number of child threads that are guaranteed to be about to start, in the sense that only the GHC scheduler
-- can continue to delay; there's no opportunity for an async exception to strike and prevent one of these threads
-- from starting.
pattern Open :: Int
pattern Open <- ((>= 0) -> True)

-- The scope is closing.
pattern Closing :: Int
pattern Closing = -1

-- The scope is closed.
pattern Closed :: Int
pattern Closed = -2

{-# COMPLETE Open, Closing, Closed #-}

-- Internal async exception thrown by a parent thread to its children when the scope is closing.
--
-- In various places we trust without verifying that any 'ScopeClosing' exception, which is not exported by this module,
-- was indeed thrown to a thread by its parent. It is possible to write a program that violates this (just catch the
-- async exception and throw it to some other thread)... but who would do that?
data ScopeClosing
  = ScopeClosing

instance Show ScopeClosing where
  show _ = "<<internal ki exception: scope closing>>"

instance Exception ScopeClosing where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

-- | Open a scope, perform an IO action with it, then close the scope.
--
-- ==== __👉 Details__
--
-- * The thread that creates a scope is considered the parent of all threads created within it.
--
-- * A scope is only valid during the callback provided to 'Ki.scoped'.
--
-- * When a scope closes (/i.e./ just before 'Ki.scoped' returns):
--
--     * The parent thread raises an exception in all of its living children.
--     * The parent thread blocks until those threads terminate.
scoped :: (Scope -> IO a) -> IO a
scoped action = do
  scope@Scope {childExceptionVar, childrenVar, statusVar} <- allocateScope

  uninterruptibleMask \restore -> do
    result <- try (restore (action scope))

    !runningChildren <- do
      atomically do
        -- Block until we haven't committed to starting any threads. Without this, we may create a thread concurrently
        -- with closing its scope, and not grab its thread id to throw an exception to.
        starting <- readTVar statusVar
        assertM (starting >= 0)
        guard (starting == 0)
        -- Indicate that this scope is closing, so attempts to create a new thread within it will throw ScopeClosing
        -- (as if the calling thread was a parent of this scope, which it should be, and we threw it a ScopeClosing
        -- ourselves).
        writeTVar statusVar Closing
        -- Return the list of currently-running children to kill. Some of them may have *just* started (e.g. if we
        -- initially retried in `guard (n == 0)` above). That's fine - kill them all!
        readTVar childrenVar

    -- Deliver a ScopeClosing exception to every running child.
    --
    -- This happens to throw in the order the children were created, but that isn't an important/useful enough feature
    -- to be worth documenting, so users shouldn't rely on it. It's definitely not the case that child 1 will completely
    -- terminate before child 2 is delivered an exception: each child may delay arbitrarily while cleaning up.
    for_ runningChildren \child -> throwTo child ScopeClosing

    atomically do
      -- Block until all children have terminated; this relies on children respecting the async exception, which they
      -- must, for correctness. Otherwise, a thread could indeed outlive the scope in which it's created, which is
      -- definitely not structured concurrency!
      children <- readTVar childrenVar
      guard (IntMap.Lazy.null children)

      -- Record the scope as closed (from closing), so subsequent attempts to use it will throw a runtime exception
      writeTVar statusVar Closed

    -- By now there are three sources of exception:
    --
    --   1) A sync or async exception thrown during the callback, captured in `result`. If applicable, we want to peel
    --      the `Propagating` off of this, which was only used to indicate it came from one of our children.
    --
    --   2) A sync or async exception left for us in `childExceptionVar` by a child that tried to propagate it to us
    --      directly, but failed (because we killed it concurrently).
    --
    --   3) An async exception waiting in our exception queue, because we still have async exceptions uninterruptibly
    --      masked.
    --
    -- We cannot throw more than one, so throw them in that priority order.
    case result of
      Left exception -> throwIO (peelOffPropagating exception)
      Right value ->
        tryTakeMVar childExceptionVar >>= \case
          Nothing -> pure value
          Just exception -> throwIO exception

-- Allocate a new scope.
allocateScope :: IO Scope
allocateScope = do
  childExceptionVar <- newEmptyMVar
  childrenVar <- newTVarIO IntMap.Lazy.empty
  nextChildIdSupply <- IntSupply.new
  parentThreadId <- myThreadId
  statusVar <- newTVarIO 0
  pure Scope {childExceptionVar, childrenVar, nextChildIdSupply, parentThreadId, statusVar}

-- Spawn a thread in a scope, providing it its child id and a function that sets the masking state to the requested
-- masking state. The given action is called with async exceptions interruptibly masked.
spawn :: Scope -> ThreadOptions -> (Tid -> (forall x. IO x -> IO x) -> UnexceptionalIO ()) -> IO ChildIds
spawn scope@Scope {childrenVar, statusVar} options action = do
  -- Interruptible mask is enough so long as none of the STM operations below block.
  --
  -- Unconditionally set masking state to MaskedInterruptible, even though we might already be at MaskedInterruptible
  -- or MaskedUninterruptible, to avoid a branch on parentMaskingState.
  interruptiblyMasked do
    -- Record the thread as being about to start. Not allowed to retry.
    nonblockingAtomically do
      status <- nonblockingReadTVar statusVar
      assertM (status >= -2)
      case status of
        Open -> nonblockingWriteTVar' statusVar (status + 1)
        Closing -> nonblockingThrowSTM ScopeClosing
        Closed -> nonblockingThrowSTM (ErrorCall "ki: scope closed")

    childIds <- spawnChild scope options action

    -- Record the child as having started. Not allowed to retry.
    nonblockingAtomically do
      starting <- nonblockingReadTVar statusVar
      assertM (starting >= 1)
      nonblockingWriteTVar' statusVar (starting - 1)
      recordChild childrenVar childIds

    pure childIds

data ChildIds
  = ChildIds
      {-# UNPACK #-} !Tid
      {-# UNPACK #-} !ThreadId

spawnChild :: Scope -> ThreadOptions -> (Tid -> (forall x. IO x -> IO x) -> UnexceptionalIO ()) -> IO ChildIds
spawnChild scope options action = do
  childId <- IntSupply.next nextChildIdSupply
  childThreadId <-
    forkWithAffinity affinity do
      when (not (null label)) do
        childThreadId <- myThreadId
        labelThread childThreadId label

      for_ allocationLimit \bytes -> do
        setAllocationCounter (byteCountToInt64 bytes)
        enableAllocationLimit

      let -- Action that sets the masking state from the current (MaskedInterruptible) to the requested one.
          atRequestedMaskingState :: IO a -> IO a
          atRequestedMaskingState =
            case requestedChildMaskingState of
              Unmasked -> unsafeUnmask
              MaskedInterruptible -> id
              MaskedUninterruptible -> uninterruptiblyMasked

      runUnexceptionalIO (action childId atRequestedMaskingState)

      nonblockingAtomically (unrecordChild childrenVar childId)
  pure (ChildIds childId childThreadId)
  where
    Scope {childrenVar, nextChildIdSupply} = scope
    ThreadOptions {affinity, allocationLimit, label, maskingState = requestedChildMaskingState} = options
{-# INLINE spawnChild #-}

-- Record our child by either:
--
--   * Flipping `Nothing` to `Just childThreadId` (common case: we record child before it unrecords itself)
--   * Flipping `Just _` to `Nothing` (uncommon case: we observe that a child already unrecorded itself)
recordChild :: TVar (IntMap ThreadId) -> ChildIds -> NonblockingSTM ()
recordChild childrenVar (ChildIds childId childThreadId) = do
  children <- nonblockingReadTVar childrenVar
  nonblockingWriteTVar' childrenVar (IntMap.Lazy.alter (maybe (Just childThreadId) (const Nothing)) childId children)

-- Unrecord a child (ourselves) by either:
--
--   * Flipping `Just childThreadId` to `Nothing` (common case: parent recorded us first)
--   * Flipping `Nothing` to `Just undefined` (uncommon case: we terminate and unrecord before parent can record us).
unrecordChild :: TVar (IntMap ThreadId) -> Tid -> NonblockingSTM ()
unrecordChild childrenVar childId = do
  children <- nonblockingReadTVar childrenVar
  nonblockingWriteTVar' childrenVar (IntMap.Lazy.alter (maybe (Just undefined) (const Nothing)) childId children)

-- | Wait until all threads created within a scope terminate.
awaitAll :: Scope -> STM ()
awaitAll Scope {childrenVar, statusVar} = do
  children <- readTVar childrenVar
  guard (IntMap.Lazy.null children)
  status <- readTVar statusVar
  assertM (status >= -2)
  case status of
    Open -> guard (status == 0)
    Closing -> retry -- block until closed
    Closed -> pure ()

-- | Create a child thread to execute an action within a scope.
--
-- /Note/: The child thread does not mask asynchronous exceptions, regardless of the parent thread's masking state. To
-- create a child thread with a different initial masking state, use 'Ki.forkWith'.
fork :: Scope -> IO a -> IO (Thread a)
fork scope =
  forkWith scope defaultThreadOptions

-- | Variant of 'Ki.fork' for threads that never return.
fork_ :: Scope -> IO Void -> IO ()
fork_ scope =
  forkWith_ scope defaultThreadOptions

-- | Variant of 'Ki.fork' that takes an additional options argument.
forkWith :: Scope -> ThreadOptions -> IO a -> IO (Thread a)
forkWith scope opts action = do
  resultVar <- newTVarIO NoResultYet
  let done result = UnexceptionalIO (atomically (writeTVar resultVar result))
  ChildIds _ childThreadId <-
    spawn scope opts \childId masking -> do
      unexceptionalTry (masking action) >>= \case
        Failure exception -> do
          when (not (exceptionIs @ScopeClosing exception)) do
            propagateException scope childId exception
          -- even put async exceptions that we propagated. this isn't totally ideal because a caller awaiting this
          -- thread would not be able to distinguish between async exceptions delivered to this thread, or itself
          done (BadResult exception)
        Success value -> done (GoodResult value)
  let doAwait =
        readTVar resultVar >>= \case
          NoResultYet -> retry
          BadResult exception -> throwSTM exception
          GoodResult value -> pure value
  pure (makeThread childThreadId doAwait)

-- | Variant of 'Ki.forkWith' for threads that never return.
forkWith_ :: Scope -> ThreadOptions -> IO Void -> IO ()
forkWith_ scope opts action = do
  _childThreadId <-
    spawn scope opts \childId masking ->
      unexceptionalTryEither
        ( \exception ->
            when (not (exceptionIs @ScopeClosing exception)) do
              propagateException scope childId exception
        )
        absurd
        (masking action)
  pure ()

-- | Like 'Ki.fork', but the child thread does not propagate exceptions that are both:
--
-- * Synchronous (/i.e./ not an instance of 'SomeAsyncException').
-- * An instance of @e@.
forkTry :: forall e a. (Exception e) => Scope -> IO a -> IO (Thread (Either e a))
forkTry scope =
  forkTryWith scope defaultThreadOptions

data Result a
  = NoResultYet
  | BadResult !SomeException -- sync or async
  | GoodResult a

-- | Variant of 'Ki.forkTry' that takes an additional options argument.
forkTryWith :: forall e a. (Exception e) => Scope -> ThreadOptions -> IO a -> IO (Thread (Either e a))
forkTryWith scope opts action = do
  resultVar <- newTVarIO NoResultYet
  let done result = UnexceptionalIO (atomically (writeTVar resultVar result))
  ChildIds _ childThreadId <-
    spawn scope opts \childId masking -> do
      result <- unexceptionalTry (masking action)
      case result of
        Failure exception -> do
          -- then-branch explanation: if the user calls `forkTry @MyAsyncException` for some reason, we want to ignore
          -- this request and propagate the async exception. `forkTry` can only be used to catch synchronous exceptions.
          let shouldPropagate =
                if exceptionIs @e exception
                  then exceptionIs @SomeAsyncException exception
                  else not (exceptionIs @ScopeClosing exception)
          when shouldPropagate (propagateException scope childId exception)
          done (BadResult exception)
        Success value -> done (GoodResult value)
  let doAwait =
        readTVar resultVar >>= \case
          NoResultYet -> retry
          BadResult exception ->
            case fromException @e exception of
              Nothing -> throwSTM exception
              Just expectedException -> pure (Left expectedException)
          GoodResult value -> pure (Right value)
  pure (makeThread childThreadId doAwait)

-- We have a non-`ScopeClosing` exception to propagate to our parent.
--
-- If our scope has already begun closing (`statusVar` is Closing), then either...
--
--   (A) We already received a `ScopeClosing`, but then ended up trying to propagate an exception anyway, because we
--   threw a synchronous exception (or were hit by a different asynchronous exception) during our teardown procedure.
--
--   or
--
--   (B) We will receive a `ScopeClosing` imminently, because our parent has *just* finished setting `statusVar` to
--   Closing, and will proceed to throw ScopeClosing to all of its children.
--
-- If (A), our parent has asynchronous exceptions masked, so we must inform it of our exception via `childExceptionVar`
-- rather than throwTo. If (B), either mechanism would work. And because we don't if we're in case (A) or (B), we just
-- `childExceptionVar`.
--
-- And if our scope has not already begun closing (`statusVar` is not Closing), then we ought to throw our exception to
-- it. But that might fail due to either...
--
--   (C) Our parent concurrently closing the scope and sending us a `ScopeClosing`; because it has asynchronous
--   exceptions uninterruptibly masked and we only have asynchronous exception *synchronously* masked, its `throwTo`
--   will return `()`, and ours will throw that `ScopeClosing` asynchronous exception. In this case, since we now know
--   our parent is tearing down and has asynchronous exceptions masked, we again inform it via `childExceptionVar`.
--
--   (D) Some *other* non-`ScopeClosing` asynchronous exception is raised here. This is truly odd: maybe it's a heap
--   overflow exception from the GHC runtime? Maybe some other thread has smuggled our `ThreadId` out and has manually
--   thrown us an exception for some reason? Either way, because we already have an exception that we are trying to
--   propagate, we just scoot these freaky exceptions under the rug.
--
-- Precondition: interruptibly masked
propagateException :: Scope -> Tid -> SomeException -> UnexceptionalIO ()
propagateException Scope {childExceptionVar, parentThreadId, statusVar} childId exception =
  UnexceptionalIO (readTVarIO statusVar) >>= \case
    Closing -> tryPutChildExceptionVar -- (A) or (B), we don't care which
    status -> do
      assertM (status >= 0) -- we know status is Open (0+) here; can't be Closed (-2)
      loop
  where
    loop :: UnexceptionalIO ()
    loop =
      unexceptionalTry (propagate exception childId parentThreadId) >>= \case
        Failure secondException
          | exceptionIs @ScopeClosing secondException -> tryPutChildExceptionVar -- (C)
          | otherwise -> loop -- (D)
        Success _ -> pure ()

    tryPutChildExceptionVar :: UnexceptionalIO ()
    tryPutChildExceptionVar =
      UnexceptionalIO (void (tryPutMVar childExceptionVar exception))
