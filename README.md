# iris

[![GitHub CI](https://github.com/chshersh/iris/workflows/CI/badge.svg)](https://github.com/chshersh/iris/actions)
[![Hackage](https://img.shields.io/hackage/v/iris.svg?logo=haskell)](https://hackage.haskell.org/package/iris)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](LICENSE)

**Iris** is a Haskell framework for building CLI applications that follow
[Command Line Interface Guidelines](https://clig.dev/).

> ℹ️ **DISCLAIMER #1:** Currently, Iris is in experimental phase and
> mostly for early adopters. It may lack documentation or have
> significant breaking changes. We appreciate anyone's help in
> improving the documentation! At the same time, the maintainers will
> strive to provide helpful migration guides.

> ℹ️ **DISCLAIMER #2:** Iris is developed and maintained in free time
> by volunteers. The development may continue for decades or may stop
> tomorrow. You can use
> [GitHub Sponsorship](https://github.com/sponsors/chshersh) to support
> the development of this project.

## Goals

Iris development is guided by the following principles:

1. **Support [Command Line Interface Guidelines](https://clig.dev/).**
   Features or changes that violate these guidelines are not accepted
   in the project.
2. **Beginner-friendliess.** Haskell beginners should be able to build
   CLI applications with Iris. Hence, the implementation of Iris API
   that uses less fancy Haskell features are preferred. When the
   complexity is justified, the cost of introducing this extra
   complexity should be mitigated by having better documentation.
3. **Reasonable batteries-included.** Iris is not trying to be
   minimalistic as possible, it strives to provide out-of-the-box
   solutions for most common problems. But at the same time, we don't
   want to impose unnecessary heavy dependencies.
4. **Excellent documentation.** Iris documentation should be as
   helpful as possible in using the framework.

   > **NOTE:** Currently, Iris may lack documentation but there's an
   > ongoing effor to improve the situation.

## Features

Iris offers the following features:

* Automatic detection of colouring support and colour-formatting functions
* Standard CLI flags `--version` and `--numeric-version` (the latter
  is helpful for detecting required tools versions)
* Ability to check required external tools if you need e.g. `curl` or
  `git` or for your app
* Utilities to open files in a browser

## How to use?

`iris` is compatible with the following GHC
versions - [supported versions](https://matrix.hackage.haskell.org/#/package/iris)

In order to start using `iris` in your project, you
will need to set it up with these steps:

1. Add the dependency on `iris` in your project's
   `.cabal` file. For this, you should modify the `build-depends`
   section according to the below section:

   ```haskell
   build-depends:
     , base ^>= LATEST_SUPPORTED_BASE
     , iris ^>= LATEST_VERSION
   ```

2. To use this package, refer to the below example.

   ```haskell
   {-# LANGUAGE GeneralizedNewtypeDeriving #-}

   module Main (main) where

   import Control.Monad.IO.Class (MonadIO (..))

   import qualified Iris


   newtype App a = App
       { unApp :: Iris.CliApp () () a
       } deriving newtype
           ( Functor
           , Applicative
           , Monad
           , MonadIO
           )

   appSettings :: Iris.CliEnvSettings () ()
   appSettings = Iris.defaultCliEnvSettings
       { Iris.cliEnvSettingsHeaderDesc = "Iris usage example"
       , Iris.cliEnvSettingsProgDesc = "A simple 'Hello, world!' utility"
       }

   app :: App ()
   app = liftIO $ putStrLn "Hello, world!"

   main :: IO ()
   main = Iris.runCliApp appSettings $ unApp app
   ```

## For contributors

Check [CONTRIBUTING.md](https://github.com/chshersh/iris/blob/main/CONTRIBUTING.md)
for contributing guidelines.

To build the project and run the tests, use `cabal`:

```shell
cabal build all
cabal test --enable-tests --test-show-details=direct
```
