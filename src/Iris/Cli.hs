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
      -- $parserInfo
    , module Iris.Cli.ParserInfo
      -- $version
    , module Iris.Cli.Version    
    ) where

import Iris.Cli.Browse
import Iris.Cli.Version
import Iris.Cli.ParserInfo

{- $browse
CLI parsers for @--browse@ flags.
-}

{- $parserInfo
  Info needed to create a CLI parser.
-}

{- $version
CLI parsers for @--version@ and @--numeric-version@ flags.
-}
