# Change Log
All notable changes to this project will be documented in this file.

## [0.1.6.1] - 2017-08-06
### Fixed
- Build process (`Stack.hs`) works with Cabal 1.X and Cabal 2.X

## [0.1.6.0] - 2017-03-25
### Fixed
- `tools/libpath.sh` recognizes `stack`

### Changed
- Update to Libsass 3.4.3

## [0.1.5.2] - 2017-03-19
### Fixed
- Build process for dependant libraries works with Cabal 1.24 & Stack 1.4

## [0.1.5.0] - 2015-12-19
### Added
- Bindings to `sass_value_op`, `sass_import_get_imp_path` and
  `sass_import_get_abs_path`

### Changed
- Uses Libsass 3.3.2
- `sass_import_get_path` and `sass_import_get_base` have been removed from
  Libsass

## [0.1.4.0] - 2015-07-10
### Fixed
- Setup copies `libsass.a` even when Cabal-1.18 is used.

## [0.1.3.0] - 2015-06-08
### Added
- Ability to link to existing version of libsass.
- Support for building (and linking to) shared version of libsass.

### Fixed
- hlibsass may be used in ghci when linked with shared version of libsass.

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

[0.1.6.1]: https://github.com/jakubfijalkowski/hlibsass/compare/v0.1.6.0...v0.1.6.1
[0.1.6.0]: https://github.com/jakubfijalkowski/hlibsass/compare/v0.1.5.2...v0.1.6.0
[0.1.5.2]: https://github.com/jakubfijalkowski/hlibsass/compare/v0.1.5.0...v0.1.5.2
[0.1.5.0]: https://github.com/jakubfijalkowski/hlibsass/compare/v0.1.4.0...v0.1.5.0
[0.1.4.0]: https://github.com/jakubfijalkowski/hlibsass/compare/v0.1.3.0...v0.1.4.0
[0.1.3.0]: https://github.com/jakubfijalkowski/hlibsass/compare/v0.1.2.1...v0.1.3.0
[0.1.2.1]: https://github.com/jakubfijalkowski/hlibsass/compare/v0.1.2.0...v0.1.2.1
[0.1.2.0]: https://github.com/jakubfijalkowski/hlibsass/compare/v0.1.1.1...v0.1.2.0
[0.1.1.1]: https://github.com/jakubfijalkowski/hlibsass/compare/v0.1.1.0...v0.1.1.1
[0.1.1.0]: https://github.com/jakubfijalkowski/hlibsass/compare/v0.1.0.0...v0.1.1.0
