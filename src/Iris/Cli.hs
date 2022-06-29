{- |
Module                  : Iris.Cli
Copyright               : (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier : MPL-2.0
Maintainer              : Dmitrii Kovanikov <kovanikov@gmail.com>
Stability               : Experimental
Portability             : Portable

CLI options parsing.

@since 0.0.0.0
-}

module Iris.Cli
    ( -- $browse
      module Iris.Cli.Browse
      -- $version
    , module Iris.Cli.Version
    ) where

import Iris.Cli.Browse
import Iris.Cli.Version

{- $browse
CLI parsers for @--browse@ flags.
-}

{- $version
CLI parsers for @--version@ and @--numeric-version@ flags.
-}
