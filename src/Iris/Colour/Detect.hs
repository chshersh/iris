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
    ( detectColour
    ) where
import System.Environment (lookupEnv)



detectColour :: Maybe String -> IO Bool
detectColour appname = any  (/= Nothing) <$> traverse lookupEnv varnames
  where
    basevarnames = ["NO_COLOR","NO_COLOUR"]
    varnames = case (<> "_") <$> appname of
        Nothing -> basevarnames
        Just n  -> ((n <>) <$> basevarnames) <> basevarnames
