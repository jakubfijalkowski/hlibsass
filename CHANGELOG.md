# Change Log
All notable changes to this project will be documented in this file.

## [0.1.2.1] - 2015-06-02
### Changed
- `libsass/VERSION` file is generated during sdist phase.

### Fixed
- all tests pass (libsass reports correct version).

## [0.1.2.0] - 2015-06-01
### Added
- Bindings to `sass_string_is_quoted`, `sass_string_set_quoted` and
  `sass_make_qstring`.

### Changed
- libsass @ 3672661 is used.

### Fixed
- Link to compare in this changelog.

## [0.1.1.1] - 2015-04-12
### Added
- This CHANGELOG file.
- `.editorconfig` file.
- Tests for Libsass version.

### Fixed
- libsass is not specified twice in extra libraries.

## [0.1.1.0] - 2015-04-11
### Fixed
- Build process - `cabal install` now works correctly

## 0.1.0.0 - 2015-04-11
### Added
- Bindings to Libsass C API
- Basic tests

[0.1.2.1]: https://github.com/jakubfijalkowski/hlibsass/compare/v0.1.2.0...v0.1.2.1
[0.1.2.0]: https://github.com/jakubfijalkowski/hlibsass/compare/v0.1.1.1...v0.1.2.0
[0.1.1.1]: https://github.com/jakubfijalkowski/hlibsass/compare/v0.1.1.0...v0.1.1.1
[0.1.1.0]: https://github.com/jakubfijalkowski/hlibsass/compare/v0.1.0.0...v0.1.1.0
