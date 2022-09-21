{- |
Module                  : Iris.Cli.Cmd
Copyright               : (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier : MPL-2.0
Maintainer              : Dmitrii Kovanikov <kovanikov@gmail.com>
Stability               : Experimental
Portability             : Portable

Wrapper around cmd.

@since 0.0.0.0
-}

module Iris.Cli.Cmd
    (
        Cmd (..)
    ) where

import Data.Kind (Type)
import Iris.Cli.Interactive (InteractiveMode)

{- | 

Wrapper around @cmd@ with additional predefined fields
-}

data Cmd (cmd :: Type) = Cmd
    { cmdInteractiveMode :: InteractiveMode
    , cmdCmd :: cmd
    }