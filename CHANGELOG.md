# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:
- Migrate FFI to ES modules (#79 by @i-am-the-slime and @JordanMartinez)

New features:

Bugfixes:

Other improvements:
- Added `purs-tidy` formatter (#77 by @thomashoneyman)

## [v6.0.0](https://github.com/purescript-contrib/purescript-formatters/releases/tag/v6.0.0) - 2021-10-16

Breaking changes:
- Upgraded to parsing v7.0.0, replace all `StringLike` constraints with `String` type (#76 by @jamesdbrock)

## [v5.0.1](https://github.com/purescript-contrib/purescript-formatters/releases/tag/v5.0.1) - 2021-05-06

Other improvements:
- Removed unused names and declaration found by the v0.14.1 PureScript release (#68 by @PureFunctor)
- Installed dependencies directly imported into source code that were previously installed transitively and removed unused `fixed-point` dependency (#68 by @PureFunctor)

## [v5.0.0](https://github.com/purescript-contrib/purescript-formatters/releases/tag/v5.0.0) - 2021-02-26

Breaking changes:
- Added support for PureScript 0.14 and dropped support for all previous versions (#62, #64)

Bugfixes:
- Fixed rounding up when a number has trailing zeroes (#56)

Other improvements:
- Changed test library from `spec` to `assert` to avoid non-core-library dependencies
- Changed default branch to `main` from `master`
- Updated to comply with Contributors library guidelines by adding new issue and pull request templates, updating documentation, and migrating to Spago for local development and CI (#58)

## [v4.0.1](https://github.com/purescript-contrib/purescript-formatters/releases/tag/v4.0.1) - 2019-02-09

- Fixed issue where negative numbers with padding would be printed like `00-0.5` rather than `-000.5`
- Fixed issue where negative years would print similarly and would not parse

## [v4.0.0](https://github.com/purescript-contrib/purescript-formatters/releases/tag/v4.0.0) - 2018-06-26

- Updated to PureScript 0.12 (@thomashoneyman)

## [v3.0.1](https://github.com/purescript-contrib/purescript-formatters/releases/tag/v3.0.1) - 2017-12-07

Bug fixes:
- Enabled timestamp parsing @tippenein

Other changes:
- Updated README and tests @tippenein
- Removed self import @paulyoung

## [v3.0.0](https://github.com/purescript-contrib/purescript-formatters/releases/tag/v3.0.0) - 2017-09-11

- Added formats for week day (@tippenein)

## [v2.1.0](https://github.com/purescript-contrib/purescript-formatters/releases/tag/v2.1.0) - 2017-08-08

- Added `Ord` instance for `FormatterCommand`

## [v2.0.2](https://github.com/purescript-contrib/purescript-formatters/releases/tag/v2.0.2) - 2017-07-19

- Fixed 24 and `{12,0}{am,pm}` cases (#26)

## [v2.0.1](https://github.com/purescript-contrib/purescript-formatters/releases/tag/v2.0.1) - 2017-07-11

- Fixed YYYY formatting (#23

## [v2.0.0](https://github.com/purescript-contrib/purescript-formatters/releases/tag/v2.0.0) - 2017-06-27

- Added support for Intervals
- Type of `DateTime.Formatter` has changed (BREAKING)
- Type of `Number.Formatter` has changed (BREAKING)

## [v1.0.1](https://github.com/purescript-contrib/purescript-formatters/releases/tag/v1.0.1) - 2017-05-13

- Fixed `Hours24` to pad to two digits when printing

## [v1.0.0](https://github.com/purescript-contrib/purescript-formatters/releases/tag/v1.0.0) - 2017-04-21

- Updated for PureScript 0.11

## [v0.4.0](https://github.com/purescript-contrib/purescript-formatters/releases/tag/v0.4.0) - 2017-04-15

- Added `MinutesTwoDigits`, `SecondsTwoDigits`, `MillisecondsShort`, and `MillisecondsTwoDigits` (@sectore)

## [v0.3.1](https://github.com/purescript-contrib/purescript-formatters/releases/tag/v0.3.1) - 2017-03-23

- Fixed `DayOfMonthTwoDigits` to zero-pad as necessary when printing (@korayal)

## [v0.3.0](https://github.com/purescript-contrib/purescript-formatters/releases/tag/v0.3.0) - 2017-03-22

- Added support for single-digit day-of-month parsing (@negator)

## [v0.2.0](https://github.com/purescript-contrib/purescript-formatters/releases/tag/v0.2.0) - 2017-03-07

- Updated dependencies

## [v0.1.3](https://github.com/purescript-contrib/purescript-formatters/releases/tag/v0.1.3) - 2017-03-01

- Fixed `bower.json` for Pursuit publishing

## [v0.1.1](https://github.com/purescript-contrib/purescript-formatters/releases/tag/v0.1.1) - 2017-02-24

- Fixed behaviour of two-digit year formatting (@dgendill)

## [v0.1.0](https://github.com/purescript-contrib/purescript-formatters/releases/tag/v0.1.0) - 2016-11-03

- Updated for PureScript 0.10

## [0.0.5](https://github.com/purescript-contrib/purescript-formatters/releases/tag/0.0.5) - 2016-09-18

- Fixed AM/PM behavior @doolse

## [0.0.1](https://github.com/purescript-contrib/purescript-formatters/releases/tag/0.0.1) - 2016-07-21

- Initial release
