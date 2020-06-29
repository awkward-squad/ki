{-# LANGUAGE PatternSynonyms #-}

module Ki.Implicit.Context
  ( -- * Context
    Context,
    global,
    cancelled,
    cancelledSTM,
    unlessCancelled,
    pattern Ki.Context.Cancelled,
  )
where

import Ki.Concurrency
import qualified Ki.Context
import Ki.Prelude

-- | A __context__ models a program's call tree, and is used as a mechanism to propagate /cancellation/ requests to
-- every __thread__ forked within a __scope__.
--
-- Every __thread__ is provided its own __context__, which is derived from its __scope__.
--
-- A __thread__ can query whether its __context__ has been /cancelled/, which is a suggestion to perform a graceful
-- termination.
type Context =
  ?context :: Ki.Context.Context

-- | Return whether the current __context__ is /cancelled/.
--
-- __Threads__ running in a /cancelled/ __context__ should terminate as soon as possible. The returned action may be
-- used to honor the /cancellation/ request in case the __thread__ is unable or unwilling to terminate normally with a
-- value.
--
-- Sometimes, a __thread__ may terminate with a value after observing a /cancellation/ request.
--
-- @
-- 'cancelled' >>= \\case
--   Nothing -> continue
--   Just _capitulate -> do
--     cleanup
--     pure value
-- @
--
-- Other times, it may be unable to, so it should call the provided action, which throws a 'Cancelled' exception.
--
-- @
-- 'cancelled' >>= \\case
--   Nothing -> continue
--   Just capitulate -> do
--     cleanup
--     capitulate
-- @
--
-- But commonly, a thread has no explicit teardown phase, and can immediately honor a /cancellation/ request;
-- 'unlessCancelled' is useful for that.
--
-- @
-- 'unlessCancelled' continue
-- @
cancelled :: Context => IO (Maybe (IO a))
cancelled =
  atomically (optional cancelledSTM)

-- | @STM@ variant of 'cancelled'.
cancelledSTM :: Context => STM (IO a)
cancelledSTM =
  Ki.Context.cancelled ?context

global :: (Context => IO a) -> IO a
global action =
  let ?context = Ki.Context.global in action

-- | Variant of 'cancelled' that immediately capitulates if the current __context__ is /cancelled/.
--
-- @
-- 'unlessCancelled' action =
--   'cancelled' >>= \\case
--     Nothing -> action
--     Just capitulate -> capitulate
-- @
--
-- /Throws/:
--
--   * Throws 'Cancelled' if the current __context__ is /cancelled/.
unlessCancelled :: Context => IO a -> IO a
unlessCancelled =
  Ki.Context.unlessCancelled ?context
