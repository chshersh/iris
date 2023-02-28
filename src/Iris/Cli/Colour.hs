{- |
Module                  : Iris.Cli.Colour
Copyright               : (c) 2023 Dmitrii Kovanikov
SPDX-License-Identifier : MPL-2.0
Maintainer              : Dmitrii Kovanikov <kovanikov@gmail.com>
Stability               : Experimental
Portability             : Portable

CLI parser for the @--colour@ and @--no-colour@ options.

@since 0.1.0.0
-}
module Iris.Cli.Colour (
    ColourOption (..),
    colourOptionP,
) where

import Control.Applicative ((<|>))

import qualified Options.Applicative as Opt

{- | Data type that tells whether the user wants the colouring option enabled,
disabled or autodetected.

See 'colourOptionP' for the parser of this option.

@since 0.1.0.0
-}
data ColourOption
    = -- | @since 0.1.0.0
      Always
    | -- | @since 0.1.0.0
      Never
    | -- | @since 0.1.0.0
      Auto
    deriving stock
        ( Show
          -- ^ @since 0.1.0.0
        , Eq
          -- ^ @since 0.1.0.0
        , Ord
          -- ^ @since 0.1.0.0
        , Enum
          -- ^ @since 0.1.0.0
        , Bounded
          -- ^ @since 0.1.0.0
        )

{- | A CLI option parser for the desired coloured output mode in the terminal.

It parses @--colour@ and @--no-colour@ flags explicitly. Otherwise, it defaults
to 'Auto'.

@since 0.1.0.0
-}
colourOptionP :: Opt.Parser ColourOption
colourOptionP = alwaysP <|> neverP <|> pure Auto
  where
    alwaysP =
        Opt.flag' Always (Opt.long "colour" <> Opt.help "Always output colours")
            <|> Opt.flag' Always (Opt.long "color" <> Opt.internal)

    neverP =
        Opt.flag' Never (Opt.long "no-colour" <> Opt.help "Never output colours")
            <|> Opt.flag' Never (Opt.long "no-color" <> Opt.internal)
            <|> Opt.flag' Never (Opt.long "disable-color" <> Opt.internal)
            <|> Opt.flag' Never (Opt.long "disable-coulor" <> Opt.internal)
