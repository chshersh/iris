{- |
Module                  : Iris.Settings
Copyright               : (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier : MPL-2.0
Maintainer              : Dmitrii Kovanikov <kovanikov@gmail.com>
Stability               : Experimental
Portability             : Portable

Environment of a CLI app.

@since 0.0.0.0
-}


module Iris.Settings
    ( -- * Settings for the CLI app
      CliEnvSettings (..)
    , defaultCliEnvSettings
    , Cmd (..)
    ) where

import Data.Kind (Type)
import qualified Options.Applicative as Opt

import Iris.Cli.Version (VersionSettings)
import Iris.Cli.Interactive (InteractiveMode)
import Iris.Tool (Tool)

{- | 

Wrapper around @cmd@ with additional predefined fields
-}

data Cmd (cmd :: Type) = Cmd
    { cmdInteractiveMode :: InteractiveMode
    , cmdCmd :: cmd
    }

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

      -- | @since 0.0.0.0
    , cliEnvSettingsRequiredTools   :: [Tool cmd]
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
    , cliEnvSettingsRequiredTools   = []
    }
