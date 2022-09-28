module Test.Iris (irisSpec) where

import Test.Hspec (Spec, describe)

import Test.Iris.Tool (toolSpec)
import Test.Iris.Cli (cliSpec)

irisSpec :: Spec
irisSpec = describe "Iris" $ do
    toolSpec
    cliSpec
