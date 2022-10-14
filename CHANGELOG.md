# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [PVP Versioning][1]. The changelog is also
available [on GitHub][2].

## [Unreleased]

## [0.0.0.0] — 2022-08-09 🌇

Initial release prepared by [@chshersh](https://github.com/chshersh).

### Added

- [#34](https://github.com/chshersh/iris/issues/34):
  Add the `--no-input` CLI option for disabling interactivity
  (by [@charrsky](https://github.com/charrsky))
- [#36](https://github.com/chshersh/iris/issues/36):
  Support Windows and macOS
  (by [@charrsky](https://github.com/charrsky))
- [#37](https://github.com/chshersh/iris/issues/37):
  Support GHC 9.0.2
  (by [@charrsky](https://github.com/charrsky))
- [#38](https://github.com/chshersh/iris/issues/38):
  Support GHC 9.2.3 and GHC 9.2.4
  (by [@charrsky](https://github.com/charrsky), [@chshersh](https://github.com/chshersh))
- [#42](https://github.com/chshersh/iris/issues/42),
  [#52](https://github.com/chshersh/iris/issues/52):
  Add `stack` support and instructions to build with `stack`
  (by [@charrsky](https://github.com/charrsky), [@chshersh](https://github.com/chshersh))
- [#43](https://github.com/chshersh/iris/issues/43):
  Add `MonadUnliftIO` instance for the `CliApp` monad
  (by [@charrsky](https://github.com/charrsky))

### Changed

- [#90](https://github.com/chshersh/iris/issues/90):
  Use Data.Text instead of ByteString for coloured output

[1]: https://pvp.haskell.org
[2]: https://github.com/chshersh/iris/releases

[Unreleased]: https://github.com/chshersh/iris/compare/v0.0.0.0...HEAD
[0.0.0.0]: https://github.com/chshersh/iris/releases/tag/v0.0.0.0
