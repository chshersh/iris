{- |
Module                  : Iris
Copyright               : (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier : MPL-2.0
Maintainer              : Dmitrii Kovanikov <kovanikov@gmail.com>
Stability               : Experimental
Portability             : Portable

__Iris__ is a Haskell CLI framework. It contains batteries for bulding
CLI applications in Haskell by following best-practices.

The library is designed for __qualified__ imports. To use it, import
like this:

@
__import qualified__ "Iris"
@

To create an CLI application with __Iris__, you need to do the
following steps:

1. Create settings for your applications by constructing a value of
   type 'CliEnvSettings'.
2. Define a monad for your application with the help of 'CliApp' by
   using either __type__ or __newtype__.

That's all! Now, you can write your CLI app by having access to all
capabilities provided by __Iris__ 🎉

@since 0.0.0.0
-}

module Iris
    ( -- $app
      module Iris.App
      -- $browse
    , module Iris.Browse
      -- $cli
    , module Iris.Cli
      -- $colour
    , module Iris.Colour
      -- $env
    , module Iris.Env
      -- $tool
    , module Iris.Tool
    ) where

import Iris.App
import Iris.Browse
import Iris.Cli
import Iris.Colour
import Iris.Env
import Iris.Tool

{- $app
CLI Application monad.
-}

{- $browse
Functions to open local files in a browser.
-}

{- $cli
CLI parsing utilities.
-}

{- $colour
Functions to detect terminal support for colouring and print coloured output.
-}

{- $env
Global environment for a CLI application and CLI app settings.
-}

{- $tool
Capabilities to check required tools on the application start.
-}
