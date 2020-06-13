# ki

[![GitHub CI](https://github.com/mitchellwrosen/ki/workflows/CI/badge.svg)](https://github.com/mitchellwrosen/ki/actions)
[![Hackage](https://img.shields.io/hackage/v/ki.svg?label=ki&logo=haskell)](https://hackage.haskell.org/package/ki-0/candidate)

`ki` is a lightweight structured-concurrency library inspired by
[`libdill`](http://libdill.org/), [`trio`](https://github.com/python-trio/trio),
[`Go Concurrency Patterns: Context`](https://blog.golang.org/context), and the
[`.NET 4 Cancellation Framework`](https://devblogs.microsoft.com/pfxteam/net-4-cancellation-framework/).

---

`ki`'s primary abstraction is the **scope**, which delimits the lifetime of **threads** forked within it.

A **thread** cannot outlive its **scope**, which brings some _structure_ to concurrent programs: when such a **scope**
"dedents", all **threads** forked anywhere within it are guaranteed to have terminated.

This structure is preserved by the function abstraction: by the time a function returns, any **threads** it may have
forked internally have finished, too. This isn't true for the built-in **thread** management functions, nor popular
abstractions built on top like [`async`](https://hackage.haskell.org/package/async), which allow a **thread** to be
backgrounded and survive longer than its calling context.

```haskell
scoped :: Context => (Scope -> IO a) -> IO a

async :: Scope -> (Context => IO a) -> IO (Thread a)
```

When the callback that introduces a **scope** ends, all **threads** forked within it are killed.

If a **thread** throws an _unexpected_ (more on that later) exception, first all other **threads** forked within its
**scope** are killed, then the exception is propagated up the call stack. In this way, a **scope** is like the root of a
call _tree_.

Normally, though, you just want to fork some **threads** and wait for them to finish. A **scope** can be waited on, with
an optional time limit.

```haskell
wait :: Scope -> IO ()

waitFor :: Scope -> Seconds -> IO ()
```

Putting it all together, the basic usage of `ki` is as follows.

```haskell
Ki.scoped \scope -> do
  Ki.async_ scope worker1
  Ki.async_ scope worker2
  Ki.wait scope
```

---

`ki` includes a notion of **scope** cancellation, which is a cooperative mechanism for gracefully terminating
computation.

```haskell
cancel :: Scope -> IO ()
```

Cancellation is hierarchical: when a **scope** is cancelled, all nested **scopes** are cancelled simultaneously. All
**threads** forked within any of them can observe the cancellation by polling.

```haskell
cancelled :: Context => IO (Maybe CancelToken)
```

After observing a `CancelToken`, a **thread** should perform a graceful termination, and then return a value, or if it
is unable to do so, throw a `Cancelled` exception with the observed `CancelToken`.

```haskell
newtype Cancelled
  = Cancelled CancelToken
  deriving stock (Eq, Show)
  deriving anyclass (Exception)
```

It's unwise to trust **threads** to notice a cancellation request at all, or perform their graceful terminations in a
timely manner. You may want to first cancel a **scope**, then wait for a finite amount of time for all **threads** to
finish, then kill the remaining ones.

Here's an example of that.

```haskell
Ki.scoped \scope -> do
  Ki.async_ scope worker1
  Ki.async_ scope worker2
  Ki.cancel scope
  Ki.waitFor 10 scope
```

Hierarchical cancellation is implemented by threading a **context** through all points in a program where a **thread**
is forked.

---

The implementation is tested for deadlocks, race conditions, and other concurrency anomalies by
[`dejafu`](http://hackage.haskell.org/package/dejafu), a fantastic unit-testing library for concurrent programs.

For optimal performance, the implementation is written against a module signature that is instantiated by primitive IO
operations for the public library component, and instantiated by the mock `dejafu` types for the test suite component.

---

Recommended reading:

  * https://vorpus.org/blog/timeouts-and-cancellation-for-humans/
  * https://vorpus.org/blog/notes-on-structured-concurrency-or-go-statement-considered-harmful/
  * http://250bpm.com/blog:124
  * http://250bpm.com/blog:137
  * http://250bpm.com/blog:139
  * http://250bpm.com/blog:146
  * http://libdill.org/structured-concurrency.html
