{- |
Module                  : Iris.Cli.Interactive
Copyright               : (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier : MPL-2.0
Maintainer              : Dmitrii Kovanikov <kovanikov@gmail.com>
Stability               : Experimental
Portability             : Portable

Interactive mode datatype and CLI parser.

@since 0.0.0.0
-}
module Iris.Cli.Interactive (
    InteractiveMode (..),
    interactiveModeP,
    handleInteractiveMode,
) where

import Options.Applicative ((<|>))
import qualified Options.Applicative as Opt
import System.Console.ANSI (hSupportsANSI)
import System.IO (stdin)

{- | Datatype for specifying if the terminal is interactive.

@since 0.0.0.0
-}
data InteractiveMode
    = -- | @since 0.0.0.0
      Interactive
    | -- | @since 0.0.0.0
      NonInteractive
    deriving stock
        ( Show
          -- ^ @since 0.0.0.0
        , Eq
          -- ^ @since 0.0.0.0
        )

{- | A CLI option parser for switching to non-interactive mode
if the @--no-input@ flag is passed.

@since 0.0.0.0
-}
interactiveModeP :: Opt.Parser InteractiveMode
interactiveModeP = nonInteractiveP <|> pure Interactive
  where
    nonInteractiveP :: Opt.Parser InteractiveMode
    nonInteractiveP =
        Opt.flag' NonInteractive $
            mconcat
                [ Opt.long "no-input"
                , Opt.help "Enter the terminal in non-interactive mode"
                ]

{- | Forces non interactive mode when the terminal is not interactive

Use this function to check whether you can get input from the terminal:

@
'handleInteractiveMode' requestedInteractiveMode
@

If the terminal is non interactive i.e. the program is run in a pipe,
interactive mode is set to false no matter what

@since 0.1.0.0
-}
handleInteractiveMode :: InteractiveMode -> IO InteractiveMode
handleInteractiveMode optionMode = do
    supportsANSI <- hSupportsANSI stdin
    pure $ if supportsANSI then optionMode else NonInteractive
