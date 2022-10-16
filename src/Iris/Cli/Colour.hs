{- |
Module                  : Iris.Cli.Colour
Copyright               : (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier : MPL-2.0
Maintainer              : Dmitrii Kovanikov <kovanikov@gmail.com>
Stability               : Experimental
Portability             : Portable

CLI options parsing for @--Colour@ and @--Colour=<FILE_PATH>@.

@since x.x.x.x
-}

module Iris.Cli.Colour
    ( ColourOption(..)
    , colourModeP
    ) where

import Control.Applicative ((<|>))
import qualified Options.Applicative as Opt


{- | Data type that tells whether the user wants colouring
enabled, disabled or autodetected.

Actual colour mode might also depend on environment variables
like NO_COLOR / NO_COLOUR or <MY_APP_NAME>_NO_COLOR / <MY_APP_NAME>_NO_COLOUR

@since x.x.x.x
-}
data ColourOption
    -- | @since x.x.x.x
    = AlwaysColour

    -- | @since x.x.x.x
    | NeverColour
    | AutoColour
    deriving stock
        ( Show     -- ^ @since x.x.x.x
        , Eq       -- ^ @since x.x.x.x
        , Ord      -- ^ @since x.x.x.x
        , Enum     -- ^ @since x.x.x.x
        , Bounded  -- ^ @since x.x.x.x
        )

{- | A CLI option parser for the requested colour mode

@since x.x.x.x
-}
colourModeP :: Opt.Parser ColourOption
colourModeP =  colour <|> color <|> pure AutoColour
    where
        colour = Opt.option colourModeReader $ mconcat
            [ Opt.long "colour"
            , Opt.metavar "Colour mode"
            , Opt.help "Enable or disable colours"
            ]
        color = Opt.option colourModeReader $  Opt.long "color" <> Opt.internal
        colourModeReader = Opt.str >>= readColourOption
        readColourOption::String -> Opt.ReadM ColourOption
        readColourOption = \case
            "auto"   -> return AutoColour
            "never"  -> return NeverColour
            "always" -> return AlwaysColour
            _        -> Opt.readerError "Accepted colour modes are 'always', 'never' and 'auto'."
