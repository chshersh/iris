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
    , interactiveModeP
    ) where

import Options.Applicative ( (<|>) )
import qualified Options.Applicative as Opt

{- Datatype for specifying if the terminal is interactive.

@since 0.0.0.0
-}
data InteractiveMode
    -- | @since 0.0.0.0
    = Interactive
    -- | @since 0.0.0.0
    | NonInteractive
    deriving stock
        ( Show       -- ^ @since 0.0.0.0
        , Eq         -- ^ @since 0.0.0.0
        )


{- | A CLI option parser for switching to non-interactive mode if the flag is passed.

@since 0.0.0.0
-}
interactiveModeP :: Opt.Parser InteractiveMode
interactiveModeP = nonInteractiveP <|> pure Interactive
  where
    nonInteractiveP :: Opt.Parser InteractiveMode
    nonInteractiveP = Opt.flag' NonInteractive $ mconcat
        [ Opt.long "no-input"
        , Opt.help "Enter the terminal in non-interactive mode"
        ]
