# ki

`ki` is a lightweight structured-concurrency library inspired by
[`libdill`](http://libdill.org/), [`trio`](https://github.com/python-trio/trio),
and [`golang.org/pkg/context`](https://golang.org/pkg/context/).

The primary abstraction is the **scope**, which delimits the lifetime of **threads**
forked within it.

```haskell
-- Create a new scope
scoped :: Context -> (Scope -> IO a) -> IO a

-- Fork a thread within a scope
async :: Scope -> (Context -> IO a) -> IO (Thread a)
```

```haskell
Ki.scoped context \scope -> do
  -- Fork three worker threads
  Ki.async_ scope worker1
  Ki.async_ scope worker2
  Ki.async_ scope worker3

  -- Block until either:
  --   * They all finish successfully
  --   * One throws an exception
  --   * Someone throws an asynchronous exception to us
  Ki.wait scope
```

A **scope** can be hard-cancelled. When the callback provided to `scoped` returns,
all remaining **threads** forked within it are killed. By the time `scoped` itself
returns, they're guaranteed to have finished.

A **scope** can be soft-cancelled, too, but it requires cooperation. A **thread**
can observe whether it's meant to gracefully terminate, but it may never notice,
or ignore the suggestion.

```haskell
-- Should I finish up?
cancelled :: Context -> IO Bool
```

```haskell
Ki.scoped context \scope -> do
  -- Fork a worker thread
  Ki.async_ scope worker

  -- Signal soft-cancellation, and block until either:
  --   * It finishes, either successfully or by throwing an exception
  --   * Someone throws an asynchronous exception to us
  --   * 1 second elapses
  Ki.waitFor scope 1000000
```

Soft-cancellation is hierarchical. It is observable by all **threads** forked
within a **scope**, all **threads** _forked by_ them, and so on.

The implementation is tested for deadlocks, race conditions, and other
concurrency anomalies by [`dejafu`](http://hackage.haskell.org/package/dejafu), a
fantastic unit-testing library for concurrent programs.
