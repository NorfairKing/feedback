# Changelog

## [0.1.0.5] - 2024-04-15

### Changed

* Fixed a bug where loops were not killed correctly.
  Now the loop commands end up in a process group and they are all stopped
  together.

## [0.1.0.4] - 2024-01-18

### Changed

* Compatibility with `optparse-applicative >=1.18`


## [0.1.0.3] - 2023-11-11

### Changed

* Fixed the autocomplete


## [0.1.0.2] - 2023-10-12

### Added

* Hooks: `before-all` and `after-first`
* Short command-line options: `c` for `--config-file` and `d` for `--debug`.

### Changed

* Any commands are now treated as script and no longer extra-escaped.
