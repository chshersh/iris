{- |
Module                  : Iris.Colour.Detect
Copyright               : (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier : MPL-2.0
Maintainer              : Dmitrii Kovanikov <kovanikov@gmail.com>
Stability               : Experimental
Portability             : Portable

Detects if colour should be enabled or not

@since x.x.x.x
-}

module Iris.Colour.Detect
    ( detectColourDisabled
    ) where
import Data.Char (toLower)
import System.Environment (lookupEnv)


detectColourDisabled :: Maybe String -> IO Bool
detectColourDisabled maybeAppName = do
      nocolour <-  any  (/= Nothing) <$> traverse lookupEnv varnames
      term <- lookupEnv "TERM"
      let dumb = (map toLower <$> term) == Just "dumb"
      return $ dumb || nocolour
  where
    basevarnames = ["NO_COLOR","NO_COLOUR"]
    prepend appName envName = appName <> "_" <> envName
    varnames = case maybeAppName of
        Nothing      -> basevarnames
        Just appName -> basevarnames <> (prepend appName <$> basevarnames)
