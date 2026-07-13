# Yaya-Unsafe

[![Hackage Version](https://img.shields.io/hackage/v/yaya-unsafe)](https://hackage.haskell.org/package/yaya-unsafe)
[![Packaging status](https://repology.org/badge/tiny-repos/haskell:yaya-unsafe.svg)](https://repology.org/project/haskell:yaya-unsafe/versions)
[![latest packaged versions](https://repology.org/badge/latest-versions/haskell:yaya-unsafe.svg)](https://repology.org/project/haskell:yaya-unsafe/versions)

Unsafe extensions to the Yaya recursion scheme library.

This includes

- `Recursive` instances for lazily-recursive types
- `Corecursive` instances for strictly-recursive types
- operations on trees that can’t be implemented in a total fashion.

## usage

### licensing

This package is licensed under [The GNU AGPL 3.0 only, with some alternatives](./LICENSE). If you need a license for usage that isn’t covered under the AGPL, please contact [Greg Pfeil](mailto:greg@technomadic.org?subject=licensing%20yaya).

You should review the [license report](docs/license-report.md) for details about dependency licenses.

### versioning

This project largely follows a strict variation of the [Haskell Package Versioning Policy](https://pvp.haskell.org/) (PVP). See [the specifics](https://sellout.github.io/flaky/haskell/strict-PVP.html).

## contributing

- [contributor guide](https://sellout.github.io/flaky/CONTRIBUTING/haskell.html)
