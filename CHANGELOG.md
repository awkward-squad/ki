# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/)
and this project adheres to the [Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [0.1.0.1] - 2020-11-30

### Changed
- Lower `cabal-version` from 3.0 to 2.2 because `stack` cannot parse 3.0
- Replace `AtomicCounter` with `Int` (to drop the `atomic-primops` dependency)

## [0.1.0] - 2020-11-11

### Added
- Initial release
