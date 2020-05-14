`gy` is a lightweight structured-concurrency library inspired by
[libdill](http://libdill.org/), [trio](https://github.com/python-trio/trio),
and [golang.org/pkg/context](https://golang.org/pkg/context/).

The primary abstraction is the *scope*, which delimits the lifetime of *threads*
forked within it.

```haskell
scoped :: Context -> (Scope -> IO a) -> IO a

async :: Scope -> (Context -> IO a) -> IO (Thread a)
```

```haskell
scoped context \scope ->
  -- Fork three worker threads
  async_ scope worker1
  async_ scope worker2
  async_ scope worker3

  -- Block until either:
  --   * They all finish successfully
  --   * One throws an exception
  --   * Someone throws an asynchronous exception to us
  wait scope
```

A *scope* can be hard-cancelled: when the callback provided to `scoped` returns,
all remaining *threads* forked within it are killed. By the time `scoped` itself
returns, they're guaranteed to have finished.

A *scope* can be soft-cancelled, too, but it requires cooperation. A *thread*
can observe whether it's meant to gracefully terminate, but it may never notice,
or ignore the suggestion.

```haskell
scoped context \scope ->
  -- Fork a worker thread
  async_ scope worker

  -- Signal soft-cancellation, and block until either:
  --   * It finishes, either successfully or by throwing an exception
  --   * Someone throws an asynchronous exception to us
  --   * 1 second elapses
  waitFor scope 1000000
```

Soft-cancellation is hierarchical: it is observable by all *threads* forked
within a *scope*, all *threads* _forked by_ *threads* within a scope, and so on.

```haskell
-- Should I finish up?
cancelled :: Context -> IO Bool
```

The implementation is tested for deadlocks, race conditions, and other
concurrency anomalies by [dejafu](http://hackage.haskell.org/package/dejafu), a
fantastic unit-testing library for concurrent programs.
