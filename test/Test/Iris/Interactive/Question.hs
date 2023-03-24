module Test.Iris.Interactive.Question (questionSpec) where

import Data.Text (Text)
import Test.Hspec (Spec, SpecWith, describe, it, shouldMatchList)

import Iris.Interactive.Question (
    -- under test
    YesNo (..),
    parseYesNo,
 )

yesAnswers :: [Text]
yesAnswers = "y" : "Y" : [y <> e <> s | y <- ["y", "Y"], e <- ["e", "E", ""], s <- ["s", "S"]]

questionSpec :: Spec
questionSpec =
    describe "Question - parse YesNo" $ do
        checkElements yesAnswers (Just Yes)
        checkElements ["n", "N", "NO", "no", "No", "nO"] (Just No)
        checkElements ["", "a", "ye", "NOone", "yesterday", "oui"] Nothing

checkElements
    :: [Text]
    -> Maybe YesNo
    -> SpecWith ()
checkElements values expected = do
    describe ("should parse to " ++ show expected) $
        forM_ values $ \strValue ->
            it (Text.unpack strValue) $
                parseYesNo strValue `shouldBe` expected
