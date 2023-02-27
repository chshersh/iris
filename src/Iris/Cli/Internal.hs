{- |
Module                  : Iris.Cli.Cmd
Copyright               : (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier : MPL-2.0
Maintainer              : Dmitrii Kovanikov <kovanikov@gmail.com>
Stability               : Internal
Portability             : Portable

Wrapper around the user-defined command.

__⚠️ This module is internal and doesn't follow PVP.__
-}
module Iris.Cli.Internal (
    Cmd (..),
) where

import Data.Kind (Type)
import Iris.Cli.Colour (ColourOption)
import Iris.Cli.Interactive (InteractiveMode)

-- | Wrapper around @cmd@ with additional predefined fields
data Cmd (cmd :: Type) = Cmd
    { cmdInteractiveMode :: InteractiveMode
    , cmdColourOption :: ColourOption
    , cmdCmd :: cmd
    }
