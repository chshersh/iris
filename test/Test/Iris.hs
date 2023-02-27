module Test.Iris (irisSpec) where

import Test.Hspec (Spec, describe)

import Test.Iris.Cli (cliSpec, cliSpecParserConflicts)
import Test.Iris.Colour (colourSpec)
import Test.Iris.Tool (toolSpec)

irisSpec :: Spec
irisSpec = describe "Iris" $ do
    cliSpec
    cliSpecParserConflicts
    colourSpec
    toolSpec
