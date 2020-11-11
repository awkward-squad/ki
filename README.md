# ki

[![GitHub CI](https://github.com/mitchellwrosen/ki/workflows/CI/badge.svg)](https://github.com/mitchellwrosen/ki/actions)
[![Hackage](https://img.shields.io/hackage/v/ki.svg?label=ki&logo=haskell)](https://hackage.haskell.org/package/ki-0/candidate)
[![Stackage LTS](https://stackage.org/package/ki/badge/lts)](https://www.stackage.org/lts/package/ki)
[![Stackage Nightly](https://stackage.org/package/ki/badge/nightly)](https://www.stackage.org/nightly/package/ki)
[![Dependencies](https://img.shields.io/hackage-deps/v/ki)](https://packdeps.haskellers.com/reverse/ki)


`ki` is a lightweight structured-concurrency library inspired by many other projects:

* [`libdill`](http://libdill.org/)
* [`trio`](https://github.com/python-trio/trio)
* [Kotlin coroutines](https://kotlinlang.org/docs/reference/coroutines-overview.html)
* [Go Concurrency Patterns: Context](https://blog.golang.org/context)
* [.NET 4 Cancellation Framework](https://devblogs.microsoft.com/pfxteam/net-4-cancellation-framework/)

## Overview

### Structured concurrency

Structured concurrency aims to make concurrent programs easier to understand by delimiting the lifetime of all
concurrently threads to a syntactic block, akin to structured programming.

This library defines five primary functions; please read the Haddocks for more comprehensive usage information.

```haskell
-- Perform an IO action within a new scope
scoped :: (Scope -> IO a) -> IO a

-- Create a background thread (propagates exceptions to its parent)
fork :: Scope -> IO a -> IO (Thread a)

-- Create a background thread (does not propagate exceptions to its parent)
async :: Scope -> IO a -> IO (Either ThreadFailed a)

-- Wait for a thread to finish
await :: Thread a -> IO a

-- Wait for all threads created within a scope to finish
wait :: Scope -> IO ()
```

A `Scope` is an explicit data structure from which threads can be created, with the property that by the time the
`Scope` itself "goes out of scope", all threads created within it will have finished.

When viewing a concurrent program as a "call tree" (analogous to a call stack), this approach, in contrast to to
directly creating green threads in the style of Haskell's `forkIO` or Golang's `go`, respects the basic function
abstraction, in that each function has a single ingress and a single egress.

Please read [Notes on structured concurrency](https://vorpus.org/blog/notes-on-structured-concurrency-or-go-statement-considered-harmful/)
for a more detailed overview on structured concurrency.

### Error propagation

When a parent thread throws or is thrown an exception, it first throws exceptions to all of its children and waits for
them to finish. This makes threads hierarchical: a thread cannot outlive the thread that created it.

When a child thread throws or is thrown an exception, depending on how it was created (see `fork` and `async` above), it
_may_ propagate the exception to its parent. This is intended to cover both of the following cases:

  * It is is _unexpected_ for a thread to fail; if it does, the program should crash loudly.
  * It is _conceivable_ for a thread to fail; if it does, this is not an exceptional circumstance, so should not require
    installing an exception handler.

### Soft-cancellation

Sometimes it is desirable to inform threads that they should endeavor to complete their work and then gracefully
terminate. This is a "cooperative" or "soft" cancellation, in contrast to throwing a thread an exception so that it
terminates immediately.

In `ki`, soft-cancellation is exposed as an alternative superset of the core API, because it involves additional
plumbing of an opaque `Context` type.

```haskell
withGlobalContext :: (Context => IO a) -> IO a

scoped :: Context => (Context => Scope -> IO a) -> IO a

fork :: Scope -> (Context => IO a) -> IO (Thread a)
```

Creating a new scope _requires_ a context, whereas the callbacks provided to `scoped` and `fork` are _provided_
a context. (Above, the context is passed around as an implicit parameter, but could instead be passed around in a
reader monad or similar).

The core API is extended with two functions to soft-cancel a scope, and to observe whether one's own scope has been
canceled.

```haskell
cancelScope :: Scope -> IO ()

cancelled :: Context => IO (Maybe CancelToken)
```

Canceling a scope is observable by all threads created within it, all threads created within _those_ threads, and so on.

#### A small soft-cancellation example

A worker thread may be written to perform a task in a loop, and cooperatively check for cancellation before doing work.

```haskell
worker :: Ki.Context => IO ()
worker =
  forever do
    checkCancellation
    doWork
  where
    checkCancellation :: IO ()
    checkCancellation = do
      maybeCancelToken <- Ki.cancelled
      case maybeCancelToken of
        Nothing -> pure ()
        Just cancelToken -> do
          putStrLn "I'm cancelled! Time to clean up."
          doCleanup
          throwIO cancelToken
```

The parent of such worker threads may (via some signaling mechanism) determine that it should cancel them, do so, and
then defensively fall back to _hard_-cancelling in case some worker is not respecting the soft-cancel signal, for
whatever reason.

```haskell
Ki.scoped \scope -> do
  worker

  -- Some time later, we decide to soft-cancel
  Ki.cancel scope

  -- Give the workers up to 10 seconds to finish
  Ki.waitFor scope (10 * Ki.seconds)

  -- Fall through the bottom of `scoped`, which throws hard-cancels all
  -- remaining threads by throwing each one an asynchronous exceptions
```

### Testing

(Some of) the implementation is tested for deadlocks, race conditions, and other concurrency anomalies by
[`dejafu`](http://hackage.haskell.org/package/dejafu), a fantastic unit-testing library for concurrent programs.

Nonetheless this library should not considered production-ready!

## Recommended reading

In chronological order of publication,

  * https://vorpus.org/blog/timeouts-and-cancellation-for-humans/
  * https://vorpus.org/blog/notes-on-structured-concurrency-or-go-statement-considered-harmful/
  * http://250bpm.com/blog:124
  * http://250bpm.com/blog:137
  * http://250bpm.com/blog:139
  * http://250bpm.com/blog:146
  * http://libdill.org/structured-concurrency.html
