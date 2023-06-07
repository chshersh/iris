module Test.Iris (irisSpec) where

import           Test.Hspec            (Spec, describe)

import           Test.Iris.Cli         (cliSpec)
import           Test.Iris.Colour      (colourSpec)
import           Test.Iris.Interactive (interactiveSpec)
import           Test.Iris.Tool        (toolSpec)

irisSpec :: Spec
irisSpec = describe "Iris" $ do
    cliSpec
    colourSpec
    toolSpec
    interactiveSpec
