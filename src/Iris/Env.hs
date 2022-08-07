{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ApplicativeDo #-}

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
      CliEnvSettings (..)
    , defaultCliEnvSettings

      -- * CLI application environment
      -- ** Constructing
    , CliEnv (..)
    , CliEnvException (..)
    , CliEnvError (..)
    , mkCliEnv

      -- ** Querying
    , asksCliEnv
    , asksAppEnv
    ) where

import Control.Exception (Exception, throwIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.Foldable (for_)
import Data.Kind (Type)
import System.IO (stderr, stdout)

import Iris.Cli.Version (VersionSettings, mkVersionParser)
import Iris.Cli.Interactive (InteractiveMode, interactiveModeP)
import Iris.Colour.Mode (ColourMode, handleColourMode)
import Iris.Tool (Tool, ToolCheckResult (..), checkTool)

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
      cliEnvCmd              :: cmd

      -- | @since 0.0.0.0
    , cliEnvStdoutColourMode :: ColourMode

      -- | @since 0.0.0.0
    , cliEnvStderrColourMode :: ColourMode

      -- | @since 0.0.0.0
    , cliEnvAppEnv           :: appEnv

      -- | @since 0.0.0.0
    , cliEnvInteractiveMode  :: InteractiveMode
    }

{- |

@since 0.0.0.0
-}
newtype CliEnvError
    -- | @since 0.0.0.0
    = CliEnvToolError ToolCheckResult
    deriving stock
        ( Show  -- ^ @since 0.0.0.0
        )

    deriving newtype
        ( Eq  -- ^ @since 0.0.0.0
        )
{- |

@since 0.0.0.0
-}
newtype CliEnvException = CliEnvException
    { unCliEnvException :: CliEnvError
    }
    deriving stock
        ( Show  -- ^ @since 0.0.0.0
        )

    deriving newtype
        ( Eq  -- ^ @since 0.0.0.0
        )

    deriving anyclass
        ( Exception  -- ^ @since 0.0.0.0
        )

{- | 

Wrapper around @cmd@ with additional predefined fields
-}

data Cmd (cmd :: Type) = Cmd
    { cmdInteractiveMode :: InteractiveMode
    , cmdCmd :: cmd
    }

{- |

__Throws:__ 'CliEnvException'

@since 0.0.0.0
-}
mkCliEnv
    :: forall cmd appEnv
    .  CliEnvSettings cmd appEnv
    -> IO (CliEnv cmd appEnv)
mkCliEnv CliEnvSettings{..} = do
    Cmd{..} <- Opt.execParser cmdParserInfo
    stdoutColourMode <- handleColourMode stdout
    stderrColourMode <- handleColourMode stderr

    for_ cliEnvSettingsRequiredTools $ \tool ->
        checkTool cmdCmd tool >>= \case
            ToolOk  -> pure ()
            toolErr -> throwIO $ CliEnvException $ CliEnvToolError toolErr

    pure CliEnv
        { cliEnvCmd              = cmdCmd
        , cliEnvStdoutColourMode = stdoutColourMode
        , cliEnvStderrColourMode = stderrColourMode
        , cliEnvAppEnv           = cliEnvSettingsAppEnv
        , cliEnvInteractiveMode  = cmdInteractiveMode
        }
  where
    cmdParserInfo :: Opt.ParserInfo (Cmd cmd)
    cmdParserInfo = Opt.info
        ( Opt.helper
        <*> mkVersionParser cliEnvSettingsVersionSettings
        <*> cmdP
        )
        $ mconcat
            [ Opt.fullDesc
            , Opt.header cliEnvSettingsHeaderDesc
            , Opt.progDesc cliEnvSettingsProgDesc
            ]
    cmdP :: Opt.Parser (Cmd cmd)
    cmdP = do
      cmdInteractiveMode <- interactiveModeP
      cmdCmd <- cliEnvSettingsCmdParser

      pure Cmd{..}

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
