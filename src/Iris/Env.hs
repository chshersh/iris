{-# LANGUAGE FlexibleContexts #-}

{- |
Module                  : Iris.Env
Copyright               : (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier : MPL-2.0
Maintainer              : Dmitrii Kovanikov <kovanikov@gmail.com>
Stability               : Experimental
Portability             : Portable

Environment of a CLI app.

@since 0.0.0.0
-}


module Iris.Env
    ( -- * Settings for the CLI app
      -- ** Global CLI settings
      CliEnvSettings (..)
    , defaultCliEnvSettings
      -- ** Application version settings
    , VersionSettings (..)
    , defaultVersionSettings

      -- * CLI application environment
      -- ** Constructing
    , CliEnv (..)
    , mkCliEnv
      -- ** Querying
    , asksCliEnv
    , asksAppEnv
    ) where

import Control.Monad.Reader (MonadReader, asks)
import Data.Kind (Type)
import Data.Version (Version, showVersion)
import System.IO (stdout)

import Iris.Colour.Mode (ColourMode, handleColourMode)

import qualified Options.Applicative as Opt


{- |

@since 0.0.0.0
-}
data CliEnvSettings (cmd :: Type) (appEnv :: Type) = CliEnvSettings
    {  -- | @since 0.0.0.0
      cliEnvSettingsCmdParser       :: Opt.Parser cmd

      -- | @since 0.0.0.0
    , cliEnvSettingsAppEnv          :: appEnv

      -- | @since 0.0.0.0
    , cliEnvSettingsHeaderDesc      :: String

      -- | @since 0.0.0.0
    , cliEnvSettingsProgDesc        :: String

      -- | @since 0.0.0.0
    , cliEnvSettingsVersionSettings :: Maybe VersionSettings
    }


{- |

@since 0.0.0.0
-}
defaultCliEnvSettings :: CliEnvSettings () ()
defaultCliEnvSettings = CliEnvSettings
    { cliEnvSettingsCmdParser       = pure ()
    , cliEnvSettingsAppEnv          = ()
    , cliEnvSettingsHeaderDesc      = "Simple CLI program"
    , cliEnvSettingsProgDesc        = "CLI tool build with iris - a Haskell CLI framework"
    , cliEnvSettingsVersionSettings = Nothing
    }

{- |

@since 0.0.0.0
-}
data VersionSettings = VersionSettings
    { -- | @since 0.0.0.0
      versionSettingsVersion :: Version

      -- | @since 0.0.0.0
    , versionSettingsMkDesc  :: String -> String
    }

{- |

@since 0.0.0.0
-}
defaultVersionSettings :: Version -> VersionSettings
defaultVersionSettings version = VersionSettings
    { versionSettingsVersion = version
    , versionSettingsMkDesc  = id
    }

{- | CLI application environment. It contains default settings for
every CLI app and parameter

Has the following type parameters:

* @cmd@ — application commands
* @appEnv@ — application-specific environment; use @()@ if you don't
  have custom app environment

@since 0.0.0.0
-}
data CliEnv (cmd :: Type) (appEnv :: Type) = CliEnv
    { -- | @since 0.0.0.0
      cliEnvCmd        :: cmd

      -- | @since 0.0.0.0
    , cliEnvColourMode :: ColourMode

      -- | @since 0.0.0.0
    , cliEnvAppEnv     :: appEnv
    }

{- |

@since 0.0.0.0
-}
mkCliEnv
    :: forall cmd appEnv
    .  CliEnvSettings cmd appEnv
    -> IO (CliEnv cmd appEnv)
mkCliEnv CliEnvSettings{..} = do
    cmd <- Opt.execParser cmdParserInfo
    colourMode <- handleColourMode stdout

    pure CliEnv
        { cliEnvCmd        = cmd
        , cliEnvColourMode = colourMode
        , cliEnvAppEnv     = cliEnvSettingsAppEnv
        }
  where
    cmdParserInfo :: Opt.ParserInfo cmd
    cmdParserInfo = Opt.info
        ( Opt.helper
        <*> mkVersionParser cliEnvSettingsVersionSettings
        <*> cliEnvSettingsCmdParser
        )
        $ mconcat
            [ Opt.fullDesc
            , Opt.header cliEnvSettingsHeaderDesc
            , Opt.progDesc cliEnvSettingsProgDesc
            ]

mkVersionParser :: Maybe VersionSettings -> Opt.Parser (a -> a)
mkVersionParser = maybe (pure id) fullVersionP

fullVersionP :: VersionSettings -> Opt.Parser (a -> a)
fullVersionP VersionSettings{..} = versionP <*> numericVersionP
  where
    versionStr :: String
    versionStr = showVersion versionSettingsVersion

    versionP :: Opt.Parser (a -> a)
    versionP = Opt.infoOption (versionSettingsMkDesc versionStr) $ mconcat
       [ Opt.long "version"
       , Opt.help "Show application version"
       ]

    numericVersionP :: Opt.Parser (a -> a)
    numericVersionP = Opt.infoOption versionStr $ mconcat
       [ Opt.long "numeric-version"
       , Opt.help "Show only numeric application version"
       ]

{- | Get a field from the global environment 'CliEnv'.

@since 0.0.0.0
-}
asksCliEnv
    :: MonadReader (CliEnv cmd appEnv) m
    => (CliEnv cmd appEnv -> field)
    -> m field
asksCliEnv = asks

{- | Get a field from custom application-specific environment
@appEnv@.

@since 0.0.0.0
-}
asksAppEnv
    :: MonadReader (CliEnv cmd appEnv) m
    => (appEnv -> field)
    -> m field
asksAppEnv getField = asksCliEnv (getField . cliEnvAppEnv)
