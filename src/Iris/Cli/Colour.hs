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
    ( colourModeP
    ) where

import Control.Applicative ((<|>))
import Data.Functor (($>))
import Iris.Cli.TripleOption
import qualified Options.Applicative as Opt


{- | A CLI option parser for the requested colour mode

@since x.x.x.x
-}
colourModeP :: Opt.Parser TripleOption
colourModeP =  colour <|> nocoulor <|> pure TOAuto
    where
        colour = (
                Opt.flag' TOAlways ( Opt.long "colour" <> Opt.help "Enable colours")
            <|> Opt.flag' TOAlways ( Opt.long "color" <> Opt.internal)
            ) $> TOAlways
        nocoulor = (
                Opt.flag' TOAlways ( Opt.long "no-colour" <> Opt.help "Disable colours")
            <|> Opt.flag' TOAlways ( Opt.long "no-color" <> Opt.internal)
            <|> Opt.flag' TOAlways ( Opt.long "disable-color" <> Opt.internal)
            <|> Opt.flag' TOAlways ( Opt.long "disable-coulor" <> Opt.internal)
            ) $> TONever
