# ki

[![GitHub CI](https://github.com/mitchellwrosen/ki/workflows/CI/badge.svg)](https://github.com/mitchellwrosen/ki/actions)
[![Hackage](https://img.shields.io/hackage/v/ki.svg?label=ki&logo=haskell)](https://hackage.haskell.org/package/ki-0/candidate)

`ki` is a lightweight structured-concurrency library inspired by

* [`libdill`](http://libdill.org/)
* [`trio`](https://github.com/python-trio/trio),
* [Kotlin coroutines](https://kotlinlang.org/docs/reference/coroutines-overview.html)
* [Go Concurrency Patterns: Context](https://blog.golang.org/context)
* [.NET 4 Cancellation Framework](https://devblogs.microsoft.com/pfxteam/net-4-cancellation-framework/).

## Tutorial

In-progress tutorial series:

- [Tutorial 1](tutorial/01.md)
- [Tutorial 2](tutorial/02.md)

## Overview

### Structured concurrency

TODO

### Soft-cancellation

TODO

### Error propagation

TODO

### Testing

The implementation is tested for deadlocks, race conditions, and other concurrency anomalies by
[`dejafu`](http://hackage.haskell.org/package/dejafu), a fantastic unit-testing library for concurrent programs.

## Recommended reading

  * https://vorpus.org/blog/timeouts-and-cancellation-for-humans/
  * https://vorpus.org/blog/notes-on-structured-concurrency-or-go-statement-considered-harmful/
  * http://250bpm.com/blog:124
  * http://250bpm.com/blog:137
  * http://250bpm.com/blog:139
  * http://250bpm.com/blog:146
  * http://libdill.org/structured-concurrency.html
