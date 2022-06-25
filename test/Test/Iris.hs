module Test.Iris (irisSpec) where

import Test.Hspec (Spec, describe)

import Test.Iris.Tool (toolSpec)

irisSpec :: Spec
irisSpec = describe "Iris" $ do
    toolSpec
