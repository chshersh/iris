{- |
Module                  : Iris.Settings
Copyright               : (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier : MPL-2.0
Maintainer              : Dmitrii Kovanikov <kovanikov@gmail.com>
Stability               : Experimental
Portability             : Portable

Settings of a CLI app environment.

You're encouraged to create a separate module @MyApp.Settings@ and put settings
for your custom application there following the below pattern:

@
__module__ MyApp.Settings (appSettings) __where__

-- data data for your CLI arguments and CLI parser
__import__ MyApp.Cli (Options, optionsP)

-- custom application environment
__import__ MyApp.Env (Env)

__import qualified__ "Iris"
__import qualified__ Paths_myapp __as__ Autogen


appSettings :: Env -> Iris.'CliEnvSettings' Options Env
appSettings env = Iris.defaultCliEnvSettings
    { -- CLI parser for Options
      Iris.'cliEnvSettingsCmdParser' = optionsP

      -- Custom app environment
    , Iris.'cliEnvSettingsAppEnv' = env

      -- Application name
    , Iris.'cliEnvSettingsAppName' =
        Just "myapp"

      -- Short app description
    , Iris.'cliEnvSettingsHeaderDesc' =
        "myapp - short description"

      -- Long app description to appear in --help
    , Iris.'cliEnvSettingsProgDesc' =
        "A tool for ..."

      -- How to print app version with the --version flag
    , Iris.'cliEnvSettingsVersionSettings' =
        Just (Iris.'Iris.Cli.Version.defaultVersionSettings' Autogen.version)
            { Iris.'Iris.Cli.Version.versionSettingsMkDesc' = \v -> "MyApp v" <> v
            }
    }
@

@since x.x.x.x
-}


module Iris.Settings
    ( -- * Settings for the CLI app
      CliEnvSettings (..)
    , defaultCliEnvSettings
    ) where

import Data.Kind (Type)
import qualified Options.Applicative as Opt
import Iris.Cli.Version (VersionSettings)

{- | The Iris settings type.

Use 'defaultCliEnvSettings' to specify only used fields.

@since 0.0.0.0
-}
data CliEnvSettings (cmd :: Type) (appEnv :: Type) = CliEnvSettings
    {  {- | @since 0.0.0.0
       -}
      cliEnvSettingsCmdParser       :: Opt.Parser cmd

      -- | @since 0.0.0.0
    , cliEnvSettingsAppEnv          :: appEnv

      -- | @since 0.0.0.0
    , cliEnvSettingsHeaderDesc      :: String

      -- | @since 0.0.0.0
    , cliEnvSettingsProgDesc        :: String

      -- | @since 0.0.0.0
    , cliEnvSettingsVersionSettings :: Maybe VersionSettings

      -- | @since x.x.x.x
    , cliEnvSettingsAppName :: Maybe String
    }


{- | Default Iris app settings.

@since 0.0.0.0
-}
defaultCliEnvSettings :: CliEnvSettings () ()
defaultCliEnvSettings = CliEnvSettings
    { cliEnvSettingsCmdParser       = pure ()
    , cliEnvSettingsAppEnv          = ()
    , cliEnvSettingsHeaderDesc      = "Simple CLI program"
    , cliEnvSettingsProgDesc        = "CLI tool build with iris - a Haskell CLI framework"
    , cliEnvSettingsAppName         = Nothing
    , cliEnvSettingsVersionSettings = Nothing
    }
