# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog 1.1](https://keepachangelog.com/en/1.1.0/),
and this project adheres to the [Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [0.3.0.5] -

## [0.3.0.4] -

## [0.3.0.3] - 2024-08-26

### Added

- support for GHC 9.10

## [0.3.0.2] -

## [0.3.0.1] -

## [0.3.0.0] -

## [0.2.1.3] – 2024–03–11

### Added

- The tests for `yaya` were previously in their own `yaya-test` package, but that has been folded into this package. Ideally, the tests would live in the `yaya` package, but Cabal doesn’t play well with circular dependencies between packages (even if the component dependencies aren’t circular).

## [0.2.1.2] -

Unknown changes.

## [0.2.1.1] -

Unknown changes.

## [0.2.1.0] -

Unknown changes.

## [0.2.0.2] – 2023–12–21

### Changed

- updated formatting for newer Ormolu

## [0.2.0.1] – 2020–05–18

### Changed

- Turned on StrictData

## [0.2.0.0] – 2020–05–14

### Changed

- updated in sync with polykind changes in yaya-0.3.0.0

## [0.1.2.2] – 2020–05–14

### Changed

- enabled and fixed warnings

## [0.1.2.1] – 2019–11–08

### Changed

- improved documentation

## [0.1.2.0] – 2019–11–08

### Added

- `law_cataCompose`

## [0.1.1.0] – 2019–01–08

### Added

- `law_anaRefl` as dual to `law_cataRefl`
- lower bounds on internal yaya dependencies

## [0.1.0.0] – 2019–01–04

### Added

- everything (this is the initial release)

[0.3.0.5]: https://github.com/sellout/no-recursion/compare/v3.2.2...v3.2.3
[0.3.0.4]: https://github.com/sellout/no-recursion/compare/v3.2.1...v3.2.2
[0.3.0.3]: https://github.com/sellout/no-recursion/compare/v3.2.0...v3.2.1
[0.3.0.2]: https://github.com/sellout/no-recursion/compare/v3.1.0...v3.2.0
[0.3.0.1]: https://github.com/sellout/no-recursion/compare/v3.0.0...v3.1.0
[0.3.0.0]: https://github.com/sellout/no-recursion/compare/v2.0.0...v3.0.0
[0.2.1.3]: https://github.com/sellout/no-recursion/compare/v1.0.0...v2.0.0
[0.2.1.2]: https://github.com/sellout/yaya/releases/tag/v1.0.0
