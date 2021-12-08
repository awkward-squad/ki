# ki

[![GitHub CI](https://github.com/awkward-squad/ki/workflows/CI/badge.svg)](https://github.com/awkward-squad/ki/actions)
[![Hackage](https://img.shields.io/hackage/v/ki.svg?label=ki&logo=haskell)](https://hackage.haskell.org/package/ki)
[![Stackage LTS](https://stackage.org/package/ki/badge/lts)](https://www.stackage.org/lts/package/ki)
[![Stackage Nightly](https://stackage.org/package/ki/badge/nightly)](https://www.stackage.org/nightly/package/ki)
[![Dependencies](https://img.shields.io/hackage-deps/v/ki)](https://packdeps.haskellers.com/reverse/ki)


`ki` is a lightweight structured-concurrency library inspired by many other projects:

* [`libdill`](http://libdill.org/)
* [`trio`](https://github.com/python-trio/trio)
* [Kotlin coroutines](https://kotlinlang.org/docs/reference/coroutines-overview.html)
* https://vorpus.org/blog/notes-on-structured-concurrency-or-go-statement-considered-harmful/
* https://250bpm.com/blog:124
* https://250bpm.com/blog:137
* https://250bpm.com/blog:139/
* https://libdill.org/structured-concurrency.html

A previous version of `ki` also included a mechanism for soft-cancellation/graceful shutdown, which took inspiration
from:

* [Go Concurrency Patterns: Context](https://blog.golang.org/context)
* [.NET 4 Cancellation Framework](https://devblogs.microsoft.com/pfxteam/net-4-cancellation-framework/)
* https://vorpus.org/blog/timeouts-and-cancellation-for-humans/
* https://250bpm.com/blog:146

However, this feature was removed (perhaps temporarily) because the design of the API was unsatisfactory.

# Documentation

[Hackage documentation](https://hackage.haskell.org/package/ki/docs/Ki.html)
