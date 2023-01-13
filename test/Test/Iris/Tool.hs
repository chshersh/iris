module Test.Iris.Tool (toolSpec) where

import Data.Text (Text)
import Data.Version (Version, makeVersion, parseVersion)
import Test.Hspec (Spec, describe, it, shouldReturn, shouldSatisfy, shouldThrow)
import Text.ParserCombinators.ReadP (readP_to_S)

import Iris.Tool (Tool (..), ToolCheckResult (..), ToolCheckError (..), ToolSelector (..), checkTool, ToolCheckException(..),
                  defaultToolSelector, need)

import qualified Data.Text as Text

ghc100 :: Tool
ghc100 = "ghc"
    { toolSelector = Just defaultToolSelector
        { toolSelectorFunction = \version -> case getVersion version of
                Nothing -> False
                Just v  -> v >= makeVersion [100]
        , toolSelectorVersionArg = Just "--numeric-version"
        }
    }

toolSpec :: Spec
toolSpec = describe "Tool" $ do
    it "should find 'curl'" $ do
        checkTool "curl" `shouldReturn` ToolOk

    it "shouldn't find 'xxx_unknown_executable'" $ do
        checkTool "xxx_unknown_executable"
            `shouldReturn` ToolCheckError (ToolNotFound "xxx_unknown_executable")

    it "shouldn't find 'ghc' version 100" $ do
        let isToolWrongVersion :: ToolCheckResult -> Bool
            isToolWrongVersion (ToolCheckError (ToolWrongVersion _)) = True
            isToolWrongVersion _other               = False

        checkTool ghc100 >>= (`shouldSatisfy` isToolWrongVersion)

    it "should not fail when 'need'ing 'curl'" $ (need ["curl"] :: IO ())

    it "should fail when 'need'ing not found tools" $ do
        let tool = "xxx_unknown_executable"
        let expectedExceptionSel (err :: ToolCheckException) = case err of
                ToolCheckException (ToolNotFound e) -> e == tool
                _other                          -> False

        need [Tool tool Nothing] `shouldThrow` expectedExceptionSel

    it "should fail when 'need'ing tools with wrong version" $ do

        let expectedExceptionSel (err :: ToolCheckException) = case err of
                ToolCheckException (ToolWrongVersion _) -> True
                _other                          -> False
        need [ghc100] `shouldThrow` expectedExceptionSel

getVersion :: Text -> Maybe Version
getVersion = extractVersion . readP_to_S parseVersion . Text.unpack
  where
    extractVersion :: [(Version, String)] -> Maybe Version
    extractVersion [(version, "")] = Just version
    extractVersion _               = Nothing
