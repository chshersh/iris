# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [PVP Versioning][1]. The changelog is also
available [on GitHub][2].

## [Unreleased]

<!-- Add new changes here -->

### Added 

[#107] (https://github.com/chshersh/iris/issues/107):
Implement 'out', 'outLn', 'err' and 'errLn' functions for outputting 'Text' to corresponding handlers

  * Add `out`, `outLn`, `err` and `errLn` functions for outputting 'Text' to corresponding handlers

  (by [@martinhelmer])
  
[#9](https://github.com/chshersh/iris/issues/9):
Implement Yes/No reading functions:

  * Add `yesno` function for asking a question with can be answered with either Yes or No
  * Add `YesNo` type (`Yes` | `No`)

  (by [@martinhelmer])

## [0.1.0.0] — 2023-03-02 🎂

### Added

- [#22](https://github.com/chshersh/iris/issues/22):
  Implement full CLI Guidelines recommendations on colouring:

    * Detect colours of `stdout` and `stderr` automatically
    * Always output colours when `--colour` (or `--color`) is specified
    * Disable colours when:
        * `--no-colour` (or `--no-color`) is specified
        * Output handle doesn't support colouring
        * `NO_COLOUR` (or `NO_COLOR`) env is set
        * `MYAPP_NO_COLOUR` (or `MYAPP_NO_COLOR`) env is set
        * `TERM=dumb` env variable is set

  (by [@marcellourbani], [@chshersh])
- [#58](https://github.com/chshersh/iris/issues/58):
  Detect non-interactive terminals automatically
  (by [@marcellourbani])
- [#89](https://github.com/chshersh/iris/issues/89)
  Add `putStderrColoured` and `putStdoutColoured` functions for putting string
  without outputting the line break
  (by [@Dponya])
- [#84](https://github.com/chshersh/iris/issues/84):
  Add support for GHC 9.4
  (by [@blackheaven])
- [#27](https://github.com/chshersh/iris/issues/27):
  Add Haddock with usage examples to many modules
  (by [@chshersh])

### Changed

- [#90](https://github.com/chshersh/iris/issues/90):
  Use `Text` instead of `ByteString` in `putStdoutColoured(Ln)` and
  `putStderrColoured(Ln)` functions
  (by [@lillycat332])
    - __Migration guide:__ Use functions from `Data.Text.Encoding` to convert
      between `ByteString` and `Text` or change your types to `Text`
- [#67](https://github.com/chshersh/iris/issues/67):
  Remove the `cliEnvSettingsRequiredTools` field from the `CliEnvSettings` type.
  Also remove the `CliEnvException` type.  Now, a new function `need` from
  `Iris.Tool` should be used for each individual command instead.
  (by [@german1608])
    - __Migration guide 1:__ Use the `need` function from the `Iris.Tool` module instead.
    - __Migration guide 2:__ Don't catch `CliEnvException` from `mkCliEnv` anymore.
- [#33](https://github.com/chshersh/iris/issues/33):
  Move errors from `ToolCheckResult` into a separate type
  (by [@charrsky])
    - __Migration guide:__ Change pattern-matching on `ToolCheckResult` to
    handle only one constructor with an error instead of previous two.

### Non-UX changes

- [#16](https://github.com/chshersh/iris/issues/16):
  Write complete Iris tutorial with a usage example using Literate Haskell
  (by [@Dponya], [@chshersh])
- [#56](https://github.com/chshersh/iris/issues/56):
  Implement tests for the `--help` parser
  (by [@CThuleHansen])
- [#57](https://github.com/chshersh/iris/issues/57):
  Add tests for the `--version` and `--numeric-version` flags
  (by [@CThuleHansen])
- [#59](https://github.com/chshersh/iris/issues/59):
  Write a test to check if global parsing conflicts with local parsing
  (by [@zetkez])
- [#70](https://github.com/chshersh/iris/issues/70):
  Add HLint configuration and CI support
  (by [@blackheaven])
- [#64](https://github.com/chshersh/iris/issues/64):
  Add Stan configuration and CI support
  (by [@blackheaven])
- [#69](https://github.com/chshersh/iris/issues/69):
  Add pre-commit hooks config
  (by [@aleeusgr])
- [#74](https://github.com/chshersh/iris/issues/74):
  Add pull request template
  (by [@himanshumalviya15])
- [#62](https://github.com/chshersh/iris/issues/62):
  Use `fourmolu` for code formatting
  (by [@chshersh])

## [0.0.0.0] — 2022-08-09 🌇

Initial release prepared by [@chshersh].

### Added

- [#34](https://github.com/chshersh/iris/issues/34):
  Add the `--no-input` CLI option for disabling interactivity
  (by [@charrsky])
- [#36](https://github.com/chshersh/iris/issues/36):
  Support Windows and macOS
  (by [@charrsky])
- [#37](https://github.com/chshersh/iris/issues/37):
  Support GHC 9.0.2
  (by [@charrsky])
- [#38](https://github.com/chshersh/iris/issues/38):
  Support GHC 9.2.3 and GHC 9.2.4
  (by [@charrsky], [@chshersh])
- [#42](https://github.com/chshersh/iris/issues/42),
  [#52](https://github.com/chshersh/iris/issues/52):
  Add `stack` support and instructions to build with `stack`
  (by [@charrsky], [@chshersh])
- [#43](https://github.com/chshersh/iris/issues/43):
  Add `MonadUnliftIO` instance for the `CliApp` monad
  (by [@charrsky])

<!-- Contributors -->

[@aleeusgr]: https://github.com/aleeusgr
[@blackheaven]: https://github.com/blackheaven
[@charrsky]: https://github.com/charrsky
[@chshersh]: https://github.com/chshersh
[@CThuleHansen]: https://github.com/CThuleHansen
[@Dponya]: https://github.com/Dponya
[@german1608]: https://github.com/german1608
[@himanshumalviya15]: https://github.com/himanshumalviya15
[@lillycat332]: https://github.com/lillycat332
[@marcellourbani]: https://github.com/marcellourbani
[@martinhelmer]: https://github.com/martinhelmer
[@zetkez]: https://github.com/zetkez

<!-- Header links -->

[1]: https://pvp.haskell.org
[2]: https://github.com/chshersh/iris/releases

<!-- Versions -->

[Unreleased]: https://github.com/chshersh/iris/compare/v0.1.0.0...HEAD
[0.1.0.0]: https://github.com/chshersh/iris/releases/tag/v0.1.0.0
[0.0.0.0]: https://github.com/chshersh/iris/releases/tag/v0.0.0.0
