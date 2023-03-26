module Test.Iris (irisSpec) where

import Test.Hspec (Spec, describe)

import Test.Iris.Cli (cliSpec, cliSpecParserConflicts)
import Test.Iris.Colour (colourSpec)
import Test.Iris.Interactive (interactiveSpec)
import Test.Iris.Tool (toolSpec)
import Test.Iris.IO (ioSpec)

irisSpec :: Spec
irisSpec = describe "Iris" $ do
    cliSpec
    cliSpecParserConflicts
    colourSpec
    toolSpec
    interactiveSpec
    ioSpec
