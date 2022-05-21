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
    ( -- * CLI application environment
      CliEnv (..)

      -- * Settings for the CLI app
    , CliEnvSettings (..)
    , defaultCliEnvSettings
    , mkCliEnv
    ) where

import Data.Kind (Type)

import qualified Options.Applicative as Opt


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
      cliEnvCmd    :: cmd

      -- | @since 0.0.0.0
    , cliEnvAppEnv :: appEnv
    }

{- |

@since 0.0.0.0
-}
data CliEnvSettings (cmd :: Type) (appEnv :: Type) = CliEnvSettings
    {  -- | @since 0.0.0.0
      cliEnvSettingsCmdParser  :: Opt.Parser cmd

      -- | @since 0.0.0.0
    , cliEnvSettingsAppEnv     :: appEnv

      -- | @since 0.0.0.0
    , cliEnvSettingsHeaderDesc :: String

      -- | @since 0.0.0.0
    , cliEnvSettingsProgDesc   :: String
    }

{- |

@since 0.0.0.0
-}
defaultCliEnvSettings :: CliEnvSettings () ()
defaultCliEnvSettings = CliEnvSettings
    { cliEnvSettingsCmdParser   = pure ()
    , cliEnvSettingsAppEnv      = ()
    , cliEnvSettingsHeaderDesc  = "Simple CLI program"
    , cliEnvSettingsProgDesc    = "CLI tool build with iris - a Haskell CLI framework"
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
    pure CliEnv
        { cliEnvCmd    = cmd
        , cliEnvAppEnv = cliEnvSettingsAppEnv
        }
  where
    cmdParserInfo :: Opt.ParserInfo cmd
    cmdParserInfo = Opt.info (Opt.helper <*> cliEnvSettingsCmdParser) $ mconcat
        [ Opt.fullDesc
        , Opt.header cliEnvSettingsHeaderDesc
        , Opt.progDesc cliEnvSettingsProgDesc
        ]
