module Test.Iris.Tool (toolSpec) where

import Data.Text (Text)
import Data.Version (Version, makeVersion, parseVersion)
import Test.Hspec (Spec, describe, it, shouldReturn, shouldSatisfy)
import Text.ParserCombinators.ReadP (readP_to_S)

import Iris.Tool (Tool (..), ToolCheckResult (..), ToolCheckError (..), ToolSelector (..), checkTool,
                  defaultToolSelector)

import qualified Data.Text as Text


toolSpec :: Spec
toolSpec = describe "Tool" $ do
    it "should find 'curl'" $ do
        checkTool () "curl" `shouldReturn` ToolOk

    it "shouldn't find 'xxx_unknown_executable'" $ do
        checkTool () "xxx_unknown_executable"
            `shouldReturn` ToolCheckError (ToolNotFound "xxx_unknown_executable")

    it "shouldn't find 'ghc' version 100" $ do
        let tool :: Tool ()
            tool = "ghc"
                { toolSelector = Just defaultToolSelector
                    { toolSelectorFunction = \_cmd version -> case getVersion version of
                          Nothing -> False
                          Just v  -> v >= makeVersion [100]
                    , toolSelectorVersionArg = Just "--numeric-version"
                    }
                }

        let isToolWrongVersion :: ToolCheckResult -> Bool
            isToolWrongVersion (ToolCheckError (ToolWrongVersion _)) = True
            isToolWrongVersion _other               = False

        checkTool () tool >>= (`shouldSatisfy` isToolWrongVersion)

getVersion :: Text -> Maybe Version
getVersion = extractVersion . readP_to_S parseVersion . Text.unpack
  where
    extractVersion :: [(Version, String)] -> Maybe Version
    extractVersion [(version, "")] = Just version
    extractVersion _               = Nothing
