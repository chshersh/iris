module Test.Iris.Common
    ( checkCI
    ) where

import Data.Maybe (isJust)
import System.Environment (lookupEnv)

checkCI :: IO Bool
checkCI = isJust <$> lookupEnv "CI"