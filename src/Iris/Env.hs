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
    ( -- * CLI application environment
      -- ** Constructing
      CliEnv (..)
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

import Iris.Cli.Internal (Cmd (..))
import Iris.Cli.ParserInfo (cmdParserInfo)
import Iris.Cli.Interactive (InteractiveMode)
import Iris.Colour.Mode (ColourMode, handleColourMode)
import Iris.Settings (CliEnvSettings (..))
import Iris.Tool (ToolCheckResult (..), checkTool, ToolCheckError (..))

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
    = CliEnvToolError ToolCheckError
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

__Throws:__ 'CliEnvException'

@since 0.0.0.0
-}
mkCliEnv
    :: forall cmd appEnv
    .  CliEnvSettings cmd appEnv
    -> IO (CliEnv cmd appEnv)
mkCliEnv cliEnvSettings@CliEnvSettings{..} = do
    Cmd{..} <- Opt.execParser $ cmdParserInfo cliEnvSettings
    stdoutColourMode <- handleColourMode stdout
    stderrColourMode <- handleColourMode stderr

    for_ cliEnvSettingsRequiredTools $ \tool ->
        checkTool cmdCmd tool >>= \case
            ToolOk  -> pure ()
            (ToolCheckError toolErr) -> throwIO $ CliEnvException $ CliEnvToolError toolErr

    pure CliEnv
        { cliEnvCmd              = cmdCmd
        , cliEnvStdoutColourMode = stdoutColourMode
        , cliEnvStderrColourMode = stderrColourMode
        , cliEnvAppEnv           = cliEnvSettingsAppEnv
        , cliEnvInteractiveMode  = cmdInteractiveMode
        }

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
