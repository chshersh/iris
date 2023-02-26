{- |
Module                  : Iris.Cli.Colour
Copyright               : (c) 2023 Dmitrii Kovanikov
SPDX-License-Identifier : MPL-2.0
Maintainer              : Dmitrii Kovanikov <kovanikov@gmail.com>
Stability               : Experimental
Portability             : Portable

CLI parser for the @--colour@ and @--no-colour@ options.

@since x.x.x.x
-}

module Iris.Cli.Colour
    ( ColourOption (..)
    , colourOptionP
    ) where

import Control.Applicative ((<|>))

import qualified Options.Applicative as Opt


{- | Data type that tells whether the user wants the colouring option enabled,
disabled or autodetected.

See 'colourOptionP' for the parser of this option.

@since x.x.x.x
-}
data ColourOption
    -- | @since x.x.x.x
    = Always

    -- | @since x.x.x.x
    | Never

    -- | @since x.x.x.x
    | Auto
    deriving stock
        ( Show     -- ^ @since x.x.x.x
        , Eq       -- ^ @since x.x.x.x
        , Ord      -- ^ @since x.x.x.x
        , Enum     -- ^ @since x.x.x.x
        , Bounded  -- ^ @since x.x.x.x
        )

{- | A CLI option parser for the desired coloured output mode in the terminal.

It parses @--colour@ and @--no-colour@ flags explicitly. Otherwise, it defaults
to 'Auto'.

@since x.x.x.x
-}
colourOptionP :: Opt.Parser ColourOption
colourOptionP =  alwaysP <|> neverP <|> pure Auto
    where
        alwaysP
            =   Opt.flag' Always ( Opt.long "colour" <> Opt.help "Always output colours")
            <|> Opt.flag' Always ( Opt.long "color" <> Opt.internal)

        neverP
            =   Opt.flag' Never ( Opt.long "no-colour" <> Opt.help "Never output colours")
            <|> Opt.flag' Never ( Opt.long "no-color" <> Opt.internal)
            <|> Opt.flag' Never ( Opt.long "disable-color" <> Opt.internal)
            <|> Opt.flag' Never ( Opt.long "disable-coulor" <> Opt.internal)
