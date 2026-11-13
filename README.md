# Yaya

[![GitHub CI](https://github.com/sellout/yaya/actions/workflows/build.yml/badge.svg)](https://github.com/sellout/yaya/actions/workflows/build.yml)
[![Nix CI](https://nix-ci.com/badge/gh:sellout:yaya)](https://nix-ci.com/gh:sellout:yaya)
[![Project Manager](https://img.shields.io/badge/%20-Project%20Manager-%235277C3?logo=nixos&labelColor=%23cccccc)](https://sellout.github.io/project-manager/)

Yet another … yet another recursion scheme library for Haskell

Total recursion scheme library for Haskell.

## overview

Recursion schemes allow you to separate _any_ recursion from your business logic, writing step-wise operations that can be applied in a way that guarantees termination (or, dually, progress).

How's this possible? You can’t have totality _and_ Turing-completeness, can you? Oh, but [you can](https://pdfs.semanticscholar.org/e291/5b546b9039a8cf8f28e0b814f6502630239f.pdf) – there is a particular type, `Partial a` (encoded with a fixed-point) that handles potential non-termination, akin to the way that `Maybe a` handles exceptional cases. It can be folded into `IO` in your main function, so that the runtime can execute a Turing-complete program that was modeled totally.

## organization

- [`yaya`](core/README.md) – the safe operations and data types for working with recursion
- [`yaya-containers`](./containers/README.md) – pattern functors and instances for the [containers](https://hackage.haskell.org/package/containers) package
- [`yaya-hedgehog`](hedgehog/README.md) – utilities for testing your Yaya-using code with [Hedgehog](https://github.com/hedgehogqa/haskell-hedgehog#readme)
- [`yaya-quickcheck`](quickcheck/README.md) – utilities for testing your Yaya-using code with [QuickCheck](https://github.com/nick8325/quickcheck#readme)
- [`yaya-unsafe`](unsafe/README.md) – unsafe instances and operations

## usage

See [the package README](./core/README.md) for usage information.

## contributing

- [contributor guide](https://sellout.github.io/flaky/CONTRIBUTING/haskell.html)

## comparisons

See [the package README](./core/README.md#comparisons) for comparisons with other similar projects.
