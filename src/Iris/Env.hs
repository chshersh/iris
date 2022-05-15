{- |
Module                  : Iris.Env
Copyright               : (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier : MPL-2.0
Maintainer              : Dmitrii Kovanikov <kovanikov@gmail.com>
Stability               : Stable
Portability             : Portable

Environment of a CLI app.

@since 0.0.0.0
-}


module Iris.Env
    ( CliEnv (..)
    ) where

import Data.Kind (Type)

import qualified Options.Applicative as Opt


{- | CLI application environment. It contains default settings for
every CLI app and parameter

Has the following type parameters:

* @cmd@ — application commands
* @appEnv@ — application specific environment; use @()@ if you don't
  have custom app environment

@since 0.0.0.0
-}
data CliEnv (cmd :: Type) (appEnv :: Type) = CliEnv
    { cliEnvCmdParser :: Opt.Parser cmd
    , cliEnvAppEnv    :: appEnv
    }
