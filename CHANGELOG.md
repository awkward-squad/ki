# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/)
and this project adheres to the [Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

### Added
- `Ki.Reader` module

### Changed
- Tweak `CancelToken` propagation semantics: now, a thread will propagate `CancelToken` if its scope was not directly
  cancelled, but instead inherited its cancellation from an ancestor scope.
- Make `CancelToken` an asynchronous exception
- Make `async` propagate async exceptions

### Fixed
- Fix small memory leak related to closing a scope
- Fix subtle bug related to GHC's treatment of deadlocked threads

## [0.2.0.1] - 2020-12-20

### Changed
- Marked dejafu test suite as "not buildable" by default

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