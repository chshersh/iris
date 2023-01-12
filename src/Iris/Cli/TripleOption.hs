{- |
Module                  : Iris.Cli.TripleOption
Copyright               : (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier : MPL-2.0
Maintainer              : Dmitrii Kovanikov <kovanikov@gmail.com>
Stability               : Experimental
Portability             : Portable

CLI options parsing for @--Colour@ and @--Colour=<FILE_PATH>@.

@since x.x.x.x
-}

module Iris.Cli.TripleOption
    ( TripleOption(..)) where

{- | Data type that tells whether the user wants an option
enabled, disabled or autodetected.

@since x.x.x.x
-}
data TripleOption
    -- | @since x.x.x.x
    = TOAlways

    -- | @since x.x.x.x
    | TONever

    -- | @since x.x.x.x
    | TOAuto
    deriving stock
        ( Show     -- ^ @since x.x.x.x
        , Eq       -- ^ @since x.x.x.x
        , Ord      -- ^ @since x.x.x.x
        , Enum     -- ^ @since x.x.x.x
        , Bounded  -- ^ @since x.x.x.x
        )
