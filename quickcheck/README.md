# Yaya-QuickCheck

[![Hackage Version](https://img.shields.io/hackage/v/yaya-quickcheck)](https://hackage.haskell.org/package/yaya-quickcheck)
[![Packaging status](https://repology.org/badge/tiny-repos/haskell:yaya-quickcheck.svg)](https://repology.org/project/haskell:yaya-quickcheck/versions)
[![latest packaged versions](https://repology.org/badge/latest-versions/haskell:yaya-quickcheck.svg)](https://repology.org/project/haskell:yaya-quickcheck/versions)

A support library for testing Yaya-using code with [QuickCheck](https://github.com/nick8325/quickcheck#readme).

This includes various generators for trees, laws that instances of Yaya type classes must follow, etc.

**NB**: Yaya itself is largely tested with [Hedgehog](https://github.com/hedgehogqa/haskell-hedgehog#readme), but this is used for [Doctest](https://github.com/sol/doctest#readme).

## usage

### licensing

This package is licensed under [The GNU AGPL 3.0 only, with some alternatives](./LICENSE). If you need a license for usage that isn’t covered under the AGPL, please contact [Greg Pfeil](mailto:greg@technomadic.org?subject=licensing%20yaya).

You should review the [license report](docs/license-report.md) for details about dependency licenses.

### versioning

This project largely follows a strict variation of the [Haskell Package Versioning Policy](https://pvp.haskell.org/) (PVP). See [the specifics](https://sellout.github.io/flaky/haskell/strict-PVP.html).

## contributing

- [contributor guide](https://sellout.github.io/flaky/CONTRIBUTING/haskell.html)
