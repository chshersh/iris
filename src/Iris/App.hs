{-# LANGUAGE DerivingVia #-}

{- |
Module                  : Iris.Env
Copyright               : (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier : MPL-2.0
Maintainer              : Dmitrii Kovanikov <kovanikov@gmail.com>
Stability               : Experimental
Portability             : Portable

Haskell CLI framework

@since 0.0.0.0
-}

module Iris.App
    ( CliApp (..)
    , runCliApp
    , runCliAppManually
    ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT (..))

import Iris.Env (CliEnv, CliEnvSettings, mkCliEnv)


{- | Main monad for your CLI application.

@since 0.0.0.0
-}
newtype CliApp cmd appEnv a = CliApp
    { unCliApp :: CliEnv cmd appEnv -> IO a
    } deriving
        ( Functor
        -- ^ @since 0.0.0.0
        , Applicative
        -- ^ @since 0.0.0.0
        , Monad
        -- ^ @since 0.0.0.0
        , MonadIO
        -- ^ @since 0.0.0.0
        , MonadReader (CliEnv cmd appEnv)
        -- ^ @since 0.0.0.0
        ) via ReaderT (CliEnv cmd appEnv) IO

{- | Run application with settings.

@since 0.0.0.0
-}
runCliApp :: CliEnvSettings cmd appEnv -> CliApp cmd appEnv a -> IO a
runCliApp settings cliApp = do
    cliEnv <- mkCliEnv settings
    runCliAppManually cliEnv cliApp

{- | Run application by constructing 'CliEnv' settings manually.

@since 0.0.0.0
-}
runCliAppManually :: CliEnv cmd appEnv -> CliApp cmd appEnv a -> IO a
runCliAppManually cliEnv (CliApp run) = run cliEnv
