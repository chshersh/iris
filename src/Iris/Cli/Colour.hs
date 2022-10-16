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
import Data.Functor (($>))
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
colourModeP =  colour <|> nocoulor <|> pure AutoColour
    where
        colour = (
                Opt.flag' AlwaysColour ( Opt.long "colour" <> Opt.help "Enable colours")
            <|> Opt.flag' AlwaysColour ( Opt.long "color" <> Opt.internal)
            ) $> AlwaysColour
        nocoulor = (
                Opt.flag' AlwaysColour ( Opt.long "no-colour" <> Opt.help "Disable colours")
            <|> Opt.flag' AlwaysColour ( Opt.long "no-color" <> Opt.internal)
            <|> Opt.flag' AlwaysColour ( Opt.long "disable-color" <> Opt.internal)
            <|> Opt.flag' AlwaysColour ( Opt.long "disable-coulor" <> Opt.internal)
            ) $> NeverColour
