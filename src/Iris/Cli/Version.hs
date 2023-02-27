{- |
Module                  : Iris.Cli.Version
Copyright               : (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier : MPL-2.0
Maintainer              : Dmitrii Kovanikov <kovanikov@gmail.com>
Stability               : Experimental
Portability             : Portable

CLI options parsing for @--version@ and @--numeric-version@

__Enabled with config__

@since 0.0.0.0
-}
module Iris.Cli.Version (
    -- * Settings
    VersionSettings (..),
    defaultVersionSettings,

    -- * CLI parser
    fullVersionP,

    -- * Internal helpers
    mkVersionParser,
) where

import Data.Version (Version, showVersion)

import qualified Options.Applicative as Opt

{- |

@since 0.0.0.0
-}
data VersionSettings = VersionSettings
    { versionSettingsVersion :: Version
    -- ^ @since 0.0.0.0
    , versionSettingsMkDesc :: String -> String
    -- ^ @since 0.0.0.0
    }

{- |

@since 0.0.0.0
-}
defaultVersionSettings :: Version -> VersionSettings
defaultVersionSettings version =
    VersionSettings
        { versionSettingsVersion = version
        , versionSettingsMkDesc = id
        }

{- |

@since 0.0.0.0
-}
mkVersionParser :: Maybe VersionSettings -> Opt.Parser (a -> a)
mkVersionParser = maybe (pure id) fullVersionP

{- |

@since 0.0.0.0
-}
fullVersionP :: VersionSettings -> Opt.Parser (a -> a)
fullVersionP VersionSettings{..} = versionP <*> numericVersionP
  where
    versionStr :: String
    versionStr = showVersion versionSettingsVersion

    versionP :: Opt.Parser (a -> a)
    versionP =
        Opt.infoOption (versionSettingsMkDesc versionStr) $
            mconcat
                [ Opt.long "version"
                , Opt.help "Show application version"
                ]

    numericVersionP :: Opt.Parser (a -> a)
    numericVersionP =
        Opt.infoOption versionStr $
            mconcat
                [ Opt.long "numeric-version"
                , Opt.help "Show only numeric application version"
                ]
