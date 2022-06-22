## [0.3.0] - Unreleased

- Breaking: Remove `Context` type, `Ki.Implicit` module, and the ability to soft-cancel a `Scope`.
- Breaking: Remove `Duration` type and its associated API, including `waitFor` and `awaitFor`.
- Breaking: Remove `Ki.Internal` module.
- Breaking: Generalize `async` to `forkTry`.
- Breaking: Generalize `forkWithUnmask` to `forkWith`.
- Breaking: Make `fork_` take an `IO Void` rather than an `IO ()`.
- Breaking: Make `fork` create an unmasked thread, rather than inherit the parent's masking state.
- Breaking: Rename `waitSTM` to `awaitAll` (replacing the old `wait` in `IO`).

- Change: Make `scoped` kill threads in the order they were created.

- Bugfix: Fix small memory leak related to closing a scope.
- Bugfix: Fix subtle bug related to GHC's treatment of deadlocked threads.
- Bugfix: make `async` (now `forkTry`) propagate async exceptions.
- Bugfix: make `scoped` safe to run with asynchronous exceptions masked.
- Bugfix: propagate exceptions to creator of scope, not creator of thread

- Performance: Use atomic fetch-and-add rather than a `TVar` to track internal child thread ids.

## [0.2.0] - 2020-12-17

- Breaking: Remove `ThreadFailed` exception wrapper.
- Breaking: Rename `cancelScope` to `cancel`.

## [0.1.0.1] - 2020-11-30

- Misc: Replace `AtomicCounter` with `Int` to drop the `atomic-primops` dependency.

- Bounds: Lower `cabal-version` from 3.0 to 2.2 because `stack` cannot parse 3.0.

## [0.1.0] - 2020-11-11

- Initial release.
