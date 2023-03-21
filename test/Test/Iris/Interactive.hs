module Test.Iris.Interactive (interactiveSpec) where

import Test.Hspec (Spec, describe)

import Test.Iris.Interactive.Question (questionSpec)

interactiveSpec :: Spec
interactiveSpec = describe "Interactive" $ do
    questionSpec
