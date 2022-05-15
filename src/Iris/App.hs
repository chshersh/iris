{-# LANGUAGE DerivingVia #-}

{- |
Module                  : Iris.Env
Copyright               : (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier : MPL-2.0
Maintainer              : Dmitrii Kovanikov <kovanikov@gmail.com>
Stability               : Stable
Portability             : Portable

Haskell CLI framework

@since 0.0.0.0
-}

module Iris.App
    ( App (..)
    ) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT (..))

import Iris.Env (CliEnv (..))


{- | Main monad for your CLI application.

@since 0.0.0.0
-}
newtype App cmd appEnv a = App
    { unApp :: CliEnv cmd appEnv -> IO a
    } deriving
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        ) via ReaderT (CliEnv cmd appEnv) IO
