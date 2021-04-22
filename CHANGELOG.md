# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/)
and this project adheres to the [Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

### Added
- Add `Ki.Reader` module.

### Changed
- Generalize `Ki` and `Ki.Implicit` to use `MonadUnliftIO`.
- Make `CancelToken` an asynchronous exception.
- Make `async` propagate async exceptions.
- Make `scoped` return an `Either Cancelled a`, to indicate whether it honored a cancellation request.
- Make `scoped` kill threads in the order they were created.
- Bump `base` upper bound to `< 4.16`

### Fixed
- Fix small memory leak related to closing a scope.
- Fix subtle bug related to GHC's treatment of deadlocked threads.

### Removed
- Remove `dejafu` test suite (too much of a pain to maintain).

## [0.2.0.1] - 2020-12-20

### Changed
- Mark `dejafu` test suite as "not buildable" by default.

## [0.2.0] - 2020-12-17

### Changed
- Rename `cancelScope` to `cancel`.

### Removed
- Remove `ThreadFailed` exception wrapper.

## [0.1.0.1] - 2020-11-30

### Changed
- Lower `cabal-version` from 3.0 to 2.2 because `stack` cannot parse 3.0
- Replace `AtomicCounter` with `Int` (to drop the `atomic-primops` dependency)

## [0.1.0] - 2020-11-11

### Added
- Initial release
