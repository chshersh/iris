{-# LANGUAGE DerivingVia #-}

{- |
Module                  : Iris.App
Copyright               : (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier : MPL-2.0
Maintainer              : Dmitrii Kovanikov <kovanikov@gmail.com>
Stability               : Experimental
Portability             : Portable

The application monad — 'CliApp'.

Many functions in __Iris__ are polymorphic over any monad that has the 'MonadReader' constraint.

Implement your own application monad as a __newtype__ wrapper around 'CliApp' in
the following way.

@
__newtype__ App a = App
    { unApp :: Iris.'CliApp' MyOptions MyEnv a
    } __deriving newtype__
        ( 'Functor'
        , 'Applicative'
        , 'Monad'
        , 'MonadIO'
        , 'MonadUnliftIO'
        , 'MonadReader' (Iris.'CliEnv' MyOptions MyEnv)
        )
@

@since 0.0.0.0
-}
module Iris.App (
    CliApp (..),
    runCliApp,
    runCliAppManually,
) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader, ReaderT (..))

import Iris.Env (CliEnv, mkCliEnv)
import Iris.Settings (CliEnvSettings)

{- | Main monad for your CLI application.

The type variables are:

* @cmd@: the data type for your CLI arguments
* @appEnv@: custom environment for your application (can be just @()@ if you
  don't need one)
* @a@: the value inside the monadic context

@since 0.0.0.0
-}
newtype CliApp cmd appEnv a = CliApp
    { unCliApp :: CliEnv cmd appEnv -> IO a
    }
    deriving
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
        , MonadUnliftIO
          -- ^ @since 0.0.0.0
        )
        via ReaderT (CliEnv cmd appEnv) IO

{- | Run application with settings.

This function is supposed to be used in your @main@ function:

@
app :: App ()
app = ... your main application ...

main :: IO ()
main = 'runCliApp' mySettings (unApp app)
@

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
