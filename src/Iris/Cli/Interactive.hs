{- |
Module                  : Iris.Cli.Interactive
Copyright               : (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier : MPL-2.0
Maintainer              : Dmitrii Kovanikov <kovanikov@gmail.com>
Stability               : Experimental
Portability             : Portable

Interative mode datatype and CLI parser.

@since 0.0.0.0
-}

module Iris.Cli.Interactive
    ( InteractiveMode (..)
    , interactP
    ) where

import qualified Options.Applicative as Opt

data InteractiveMode = Interactive | NonInteractive deriving stock Show


{- | A CLI option parser for switching to non-interactive mode if the flag is passed.

@since 0.0.0.0
-}
interactP
    :: String  -- ^ Flag description
    -> Opt.Parser InteractiveMode
interactP description = Opt.flag' NonInteractive $ mconcat
    [ Opt.long "no-input"
    , Opt.help description
    ]