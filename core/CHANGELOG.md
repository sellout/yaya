# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the [Haskell Package Versioning Policy](https://pvp.haskell.org/).

## 0.3.0.0 – 2020–05–14
### Changed
- introduced minimal poly-kinding of type classes

## 0.2.1.3 – 2020–05–14
### Changed
- enabled and fixed warnings

## 0.2.1.2 – 2019–11–08
### Changed
- improved documentation

## 0.2.1.1 – 2019–11–08
### Added
- documentation explaining limitations of `Mu`
- tests for `law_cataCompose` (which bumps the yaya-hedgehog dependency for tests)

## 0.2.1.0 – 2019–01–08
### Added
- exports of type class methods via `Yaya.Retrofit`

## 0.2.0.0 – 2019–01–08
### Added
- `DFunctor` instances for `Mu` and `Nu`
- lower bounds on internal yaya dependencies

### Changed
- moved `DFunctor` and `HFunctor` to a new `Yaya.Functor` module
- renamed `cursiveIso` to `steppableIso`

## 0.1.0.0 – 2019–01–04
### Added
- everything (this is the initial release)
