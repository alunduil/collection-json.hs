# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog 1.1.0](https://keepachangelog.com/en/1.1.0/),
and this project adheres to the [Haskell Package Versioning Policy
(PVP)](https://pvp.haskell.org/).

## [Unreleased]

### Changed

- Widen `aeson` upper bound to allow `2.2.x`.
- Widen `base` upper bound to allow `4.22.x`.
- Widen `network-uri-json` upper bound to allow `0.4.x`.
- Widen `hspec` upper bound to allow `2.11.x` (test suite).
- Widen `network-arbitrary` upper bound to allow `1.x` (test suite).
- Widen `QuickCheck` upper bound to allow `2.18.x` (test suite).

## [1.3.1.3] - 2019-02-21

### Changed

- Update cabal.config.
- Integrate cloudbuild fixes from network-arbitrary.
- Update sdist path for new-sdist.

## [1.3.1.2] - 2018-02-05

### Changed

- Change sdist to new-sdist.

## [1.3.1.1] - 2019-01-31

### Changed

- Change TAG to TAG_NAME in cloudbuild configuration.

## [1.3.1.0] - 2019-01-22

### Changed

- Update hspec-discover.
- Update hspec.

## [1.3.0.0] - 2019-01-02

### Changed

- Update travis configuration.
- Update network-uri-json.

## [1.2.0.0] - 2018-12-28

### Added

- Add disclaimer to README.
- Add envrc to autoload environment.
- Add cloudbuild configuration to publish to hackage.

### Changed

- Update base, network-uri-json, network-arbitrary, hspec, aeson, QuickCheck,
  hspec-discover.

## [1.1.2.1] - 2018-01-20

### Changed

- Bump network-arbitrary dependency.

## [1.1.2.0] - 2018-01-06

### Changed

- Externalize network-arbitrary dependency.

## [1.1.1.0] - 2017-12-21

### Changed

- Bump dependencies on aeson and QuickCheck.

## [1.1.0.2] - 2017-11-17

### Changed

- Use `network-uri-json` package.

## [1.1.0.1] - 2017-11-11

### Added

- Add shrink methods to Arbitrary instances.

### Changed

- Reduce prop usage by using it correctly.
- Move Internal module to External.

### Removed

- Remove broken travis deployment.

### Fixed

- Fix git URL.

## [1.1.0.0] - 2017-11-05

### Added

- Add more robust URI testing.
- Add nix development environment (`shell.nix`, `default.nix`,
  `collection-json.nix`).

### Changed

- Minimize produced JSON in `ToJSON` instances.
- Attempt to ensure RFC compliance with hspec.
- Change test suite to hspec.

## [1.0.1.0] - 2017-08-08

### Added

- Add travis deployment section.

### Changed

- Increase `aeson` upper bound to 1.2.
- Increase `base` upper bound to 4.11.
- Reduce `maxSize` in quickcheck tests.
- Cleanup unused pragmas.
- Update travis configuration.

## [1.0.0.0] - 2017-08-05

### Added

- Add Property Test Suite.
- Add Code of Conduct.
- Add Travis CI Configuration.

### Changed

- Update README.
- Other Package Updates.

[Unreleased]: https://github.com/alunduil/collection-json.hs/compare/1.3.1.3...HEAD
[1.3.1.3]: https://github.com/alunduil/collection-json.hs/compare/1.3.1.2...1.3.1.3
[1.3.1.2]: https://github.com/alunduil/collection-json.hs/compare/1.3.1.1...1.3.1.2
[1.3.1.1]: https://github.com/alunduil/collection-json.hs/compare/1.3.1.0...1.3.1.1
[1.3.1.0]: https://github.com/alunduil/collection-json.hs/compare/1.3.0.0...1.3.1.0
[1.3.0.0]: https://github.com/alunduil/collection-json.hs/compare/1.2.0.0...1.3.0.0
[1.2.0.0]: https://github.com/alunduil/collection-json.hs/compare/1.1.2.1...1.2.0.0
[1.1.2.1]: https://github.com/alunduil/collection-json.hs/compare/1.1.2.0...1.1.2.1
[1.1.2.0]: https://github.com/alunduil/collection-json.hs/compare/1.1.1.0...1.1.2.0
[1.1.1.0]: https://github.com/alunduil/collection-json.hs/compare/1.1.0.2...1.1.1.0
[1.1.0.2]: https://github.com/alunduil/collection-json.hs/compare/1.1.0.1...1.1.0.2
[1.1.0.1]: https://github.com/alunduil/collection-json.hs/compare/1.1.0.0...1.1.0.1
[1.1.0.0]: https://github.com/alunduil/collection-json.hs/compare/1.0.1.0...1.1.0.0
[1.0.1.0]: https://github.com/alunduil/collection-json.hs/compare/1.0.0.0...1.0.1.0
[1.0.0.0]: https://github.com/alunduil/collection-json.hs/releases/tag/1.0.0.0
