# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog 1.1](https://keepachangelog.com/en/1.1.0/),
and this project adheres to the [Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [0.5.0.0] - 2026-01-18

### Added

- support for GHC 9.12 and 9.14
- `verify-no-recursion` Cabal flag for auditing recursion usage by the library

### Changed

- license from “AGPL-3.0-or-later” to “AGPL-3.0-only WITH
  Universal-FOSS-exception-1.0 OR LicenseRef-commercial”

### Removed

- support for GHC 8.6

## [0.4.1.4] -

## [0.4.1.3] -

## [0.4.1.2] - 2024-08-26

### Added

- support for GHC 9.10

## [0.4.1.1] -

## [0.4.1.0] -

## [0.4.0.0] -

## [0.3.3.1] – 2024–03–11

### Added

- The tests for this package were previously in their own `yaya-unsafe-test` package, but that has been folded into this package. Presumably, they were only separated to parallel the separation between `yaya` and `yaya-test`, which had better motivation.

## [0.3.3.0]

Unknown changes.

## [0.3.2.0]

Unknown changes.

## [0.3.1.0]

Unknown changes.

## [0.3.0.0]

Unknown changes.

## [0.2.0.2] – 2023–12–21

### Changed

- updated formatting for newer Ormolu

## [0.2.0.1] – 2020–05–18

### Changed

- Turned on StrictData

## [0.2.0.0] – 2020–05–14

### Changed

- updated in sync with polykinding changes in yaya-0.3.0.0

## [0.1.1.3] – 2020–05–14

### Changed

- enabled and fixed warnings

## [0.1.1.2] – 2019–11–08

### Changed

- improved documentation

## [0.1.1.1] – 2019–11–08

### Added

- tests for `law_cataCompose` (which bumps the yaya-hedgehog dependency for tests)

## [0.1.1.0] – 2019–01–08

### Added

- lower bounds on internal yaya dependencies

### Changed

- weakened constraints on a couple operations

## [0.1.0.0] – 2019–01–04

### Added

- everything (this is the initial release)

[0.5.0.0]: https://github.com/sellout/no-recursion/compare/v3.2.3...v4.0.0
[0.4.1.4]: https://github.com/sellout/no-recursion/compare/v3.2.2...v3.2.3
[0.4.1.3]: https://github.com/sellout/no-recursion/compare/v3.2.1...v3.2.2
[0.4.1.2]: https://github.com/sellout/no-recursion/compare/v3.2.0...v3.2.1
[0.4.1.1]: https://github.com/sellout/no-recursion/compare/v3.1.0...v3.2.0
[0.4.1.0]: https://github.com/sellout/no-recursion/compare/v3.0.0...v3.1.0
[0.4.0.0]: https://github.com/sellout/no-recursion/compare/v2.0.0...v3.0.0
[0.3.3.1]: https://github.com/sellout/no-recursion/compare/v1.0.0...v2.0.0
[0.3.3.0]: https://github.com/sellout/yaya/releases/tag/v1.0.0
