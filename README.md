# ki

[![GitHub CI](https://github.com/mitchellwrosen/ki/workflows/CI/badge.svg)](https://github.com/mitchellwrosen/ki/actions)
[![Hackage](https://img.shields.io/hackage/v/ki.svg?label=ki&logo=haskell)](https://hackage.haskell.org/package/ki-0/candidate)
[![Stackage LTS](https://stackage.org/package/ki/badge/lts)](https://www.stackage.org/lts/package/ki)
[![Stackage Nightly](https://stackage.org/package/ki/badge/nightly)](https://www.stackage.org/nightly/package/ki)
[![Dependencies](https://img.shields.io/hackage-deps/v/ki)](https://packdeps.haskellers.com/reverse/ki)


`ki` is a lightweight structured-concurrency library inspired by

* [`libdill`](http://libdill.org/)
* [`trio`](https://github.com/python-trio/trio)
* [Kotlin coroutines](https://kotlinlang.org/docs/reference/coroutines-overview.html)
* [Go Concurrency Patterns: Context](https://blog.golang.org/context)
* [.NET 4 Cancellation Framework](https://devblogs.microsoft.com/pfxteam/net-4-cancellation-framework/)

## Tutorial

If this is your first time here, please have a look at the tutorial, which
should (eventually) demonstrate how to use every type and function defined in
this library.

[In-progress tutorial series](tutorial/01.md)

## Overview

### Structured concurrency

Structured concurrency aims to make concurrent programs easier to understand by
requiring additional fanfare when spawning a __thread__. As explained in
[Notes on Structured Concurrency](https://vorpus.org/blog/notes-on-structured-concurrency-or-go-statement-considered-harmful/),
"goroutines" (and thus Haskell's `forkIO`) are "considered harmful".

When using this library, compared to `base`, spawning a thread takes one more
argument:

```
fork :: Scope -> IO () -> IO ()
```

In exchange for this boilerplate, you'll be programming in a world where
spawning threads respects the basic function abstraction: all threads of
execution enter a function at one place (the top), and exit the function at one
place (the bottom).

Put differently, a function is unable to silently spawn a background thread
whose lifetime extends beyond the function call itself. By the time a function
returns, any threads it may have spawned are guaranteed to have finished.

This approach encourages a controlled, hierarchical structure to the nature of a
concurrent program.

### Soft-cancellation

### Error propagation

In a synchronous setting, when an exception is thrown, it propagates up the call
stack until an appropriate exception handler is found, or else the entire
program exits.

When using this library, a concurrent program behaves similarly, with the call
stack the call stack generalized to a "call tree".

Error propagation between threads is bidirectional. There are variations on this
theme, but in general:

  * When a child thread throws or is thrown an exception, it propagates the
    exception to its parent.
  * When a parent thread throws or is thrown an exception, it first kills all of
    its children, and waits for them to finish.

This makes is much more difficult to have silent failures in background threads
that aren't supposed to crash.

### Testing

The implementation is tested for deadlocks, race conditions, and other concurrency anomalies by
[`dejafu`](http://hackage.haskell.org/package/dejafu), a fantastic unit-testing library for concurrent programs.

### Comparison to other libraries

#### `async`

#### `scheduler`

#### `slave-thread`

## Recommended reading

  * https://vorpus.org/blog/timeouts-and-cancellation-for-humans/
  * https://vorpus.org/blog/notes-on-structured-concurrency-or-go-statement-considered-harmful/
  * http://250bpm.com/blog:124
  * http://250bpm.com/blog:137
  * http://250bpm.com/blog:139
  * http://250bpm.com/blog:146
  * http://libdill.org/structured-concurrency.html
