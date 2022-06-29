{- |
Module                  : Iris.Colour
Copyright               : (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier : MPL-2.0
Maintainer              : Dmitrii Kovanikov <kovanikov@gmail.com>
Stability               : Experimental
Portability             : Portable

Functions to handle colouring of the output to terminal.

@since 0.0.0.0
-}

module Iris.Colour
    ( -- $mode
      module Iris.Colour.Mode
      -- $formatting
    , module Iris.Colour.Formatting
    ) where

import Iris.Colour.Formatting
import Iris.Colour.Mode

{- $mode
Colouring mode in application.
-}

{- $formatting
Formatting of Terminal output
-}
