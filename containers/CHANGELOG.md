# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog 1.1](https://keepachangelog.com/en/1.1.0/),
and this project adheres to the [Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [0.2.0.0] - 2026-01-18

### Added

- support for GHC 9.12 and 9.14
- `verify-no-recursion` Cabal flag for auditing recursion usage by the library
- CPP conditionalization on containers-0.8 for `IntMap` & `IntSet` support – if you want to use those types on both GHC 9.14 and earlier versions, you’ll likely need similar conditionalization

### Changed

- license from “AGPL-3.0-or-later” to “AGPL-3.0-only WITH
  Universal-FOSS-exception-1.0 OR LicenseRef-commercial”

### Removed

- support for GHC 8.6

## [0.1.2.2] -

## [0.1.2.1] - 2024-08-26

### Added

- support for GHC 9.10

## [0.1.2.0] -

## [0.1.1.0] -

## [0.1.0.2] -

## [0.1.0.0] -

### Added

- initial release of this package

[0.2.0.0]: https://github.com/sellout/no-recursion/compare/v3.2.3...v4.0.0
[0.1.2.2]: https://github.com/sellout/no-recursion/compare/v3.2.1...v3.2.3
[0.1.2.1]: https://github.com/sellout/no-recursion/compare/v3.1.0...v3.2.1
[0.1.2.0]: https://github.com/sellout/no-recursion/compare/v3.0.0...v3.1.0
[0.1.1.0]: https://github.com/sellout/no-recursion/compare/v1.0.0...v3.0.0
[0.1.0.2]: https://github.com/sellout/yaya/releases/tag/v1.0.0
