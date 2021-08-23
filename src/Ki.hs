-- | Please read "Ki.Documentation" for an overview of how to use this library.
module Ki
  ( -- * Scope
    Scope,
    scoped,
    wait,
    waitSTM,
    waitFor,

    -- * Thread
    Thread,
    fork,
    fork_,
    forkWithUnmask,
    forkWithUnmask_,
    async,
    asyncWithUnmask,
    await,
    awaitSTM,
    awaitFor,

    -- * Miscellaneous
    Duration,
    microseconds,
    milliseconds,
    seconds,
    timeoutSTM,
    sleep,
  )
where

import Ki.Duration (Duration, microseconds, milliseconds, seconds)
import Ki.Prelude
import Ki.Scope
  ( Scope,
    Thread,
    async,
    asyncWithUnmask,
    await,
    awaitFor,
    awaitSTM,
    fork,
    forkWithUnmask,
    forkWithUnmask_,
    fork_,
    scoped,
    wait,
    waitFor,
    waitSTM,
  )
import Ki.Timeout (timeoutSTM)

-- | Duration-based @threadDelay@.
sleep ::
  MonadIO m =>
  -- |
  Duration ->
  m ()
sleep duration =
  timeoutSTM duration (pure (pure ())) (pure ())
{-# SPECIALIZE sleep :: Duration -> IO () #-}
