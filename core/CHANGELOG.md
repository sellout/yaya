# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog 1.1](https://keepachangelog.com/en/1.1.0/),
and this project adheres to the [Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [0.6.2.2] - 2024-10-23

### Changed

- simplified Template Haskell internals in `Yaya.Retrofit`

## [0.6.2.1] - 2024-08-26

### Added

- support for GHC 9.10

## [0.6.2.0] -

## [0.6.1.0] -

## [0.6.0.0] -

## [0.5.0.0] -

## [0.4.2.3] – 2024–01–11

### Changed

- added CPP to support GHC 9.8

## [0.4.2.2] – 2023–12–21

### Changed

- updated formatting for newer Ormolu
- fixed a Haddock typo

## [0.4.2.1] -

Unknown changes.

## [0.4.2.0] -

Unknown changes.

## [0.4.1.0] -

Unknown changes.

## [0.4.0.1] – 2020–12–08

### Changed

- updated explanation of differences from `recursion-schemes`
- updated comments and parameter names in metamorphisms

## [0.4.0.0] – 2020–12–08

### Added

- more example algebras

### Changed

- `while` is generalized and now called `definedOrInput`
- `split` renamed to `diagonal`

## [0.3.2.0] – 2020–06–01

### Added

- `zipAlgebraMs`

## [0.3.1.2] – 2020–05–18

### Added

- `HFunctor` instances

## [0.3.1.1] – 2020–05–18

### Changed

- Turned on StrictData

## [0.3.1.0] – 2020–05–18

### Added

- Copied Kmett's auto-extraction of pattern functors

## [0.3.0.0] – 2020–05–14

### Changed

- introduced minimal poly-kinding of type classes

## [0.2.1.3] – 2020–05–14

### Changed

- enabled and fixed warnings

## [0.2.1.2] – 2019–11–08

### Changed

- improved documentation

## [0.2.1.1] – 2019–11–08

### Added

- documentation explaining limitations of `Mu`
- tests for `law_cataCompose` (which bumps the yaya-hedgehog dependency for tests)

## [0.2.1.0] – 2019–01–08

### Added

- exports of type class methods via `Yaya.Retrofit`

## [0.2.0.0] – 2019–01–08

### Added

- `DFunctor` instances for `Mu` and `Nu`
- lower bounds on internal yaya dependencies

### Changed

- moved `DFunctor` and `HFunctor` to a new `Yaya.Functor` module
- renamed `cursiveIso` to `steppableIso`

## [0.1.0.0] – 2019–01–04

### Added

- everything (this is the initial release)
