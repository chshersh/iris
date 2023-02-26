module Test.Iris.Colour (colourSpec) where

import Test.Hspec (Spec, describe)

import Test.Iris.Colour.Mode (modeSpec)

colourSpec :: Spec
colourSpec = describe "Colour" $ do
    modeSpec