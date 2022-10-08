{-# LANGUAGE ScopedTypeVariables #-}
module Test.Iris.Cli (cliSpec) where

import Test.Hspec (Spec, describe, it, shouldBe, expectationFailure, shouldReturn)

import Iris.Cli.ParserInfo (cmdParserInfo)
import Iris.Settings (defaultCliEnvSettings, cliEnvSettingsVersionSettings)
import Iris.Cli.Version (defaultVersionSettings)
import qualified Options.Applicative as Opt
import qualified Paths_iris as Autogen
import Iris.Cli (VersionSettings(versionSettingsMkDesc))
import Iris.Cli.Interactive (handleInteractiveMode, InteractiveMode (..))
import System.Environment (getEnvironment)


checkCI :: IO Bool
checkCI = (== Just "true") . lookup "CI" <$> getEnvironment

expectedHelpText :: String
expectedHelpText =
    "Simple CLI program\n\
    \\n\
    \Usage: <iris-test> [--no-input]\n\
    \\n\
    \  CLI tool build with iris - a Haskell CLI framework\n\
    \\n\
    \Available options:\n\
    \  -h,--help                Show this help text\n\
    \  --no-input               Enter the terminal in non-interactive mode"

expectedHelpTextWithVersion :: String
expectedHelpTextWithVersion =
    "Simple CLI program\n\
    \\n\
    \Usage: <iris-test> [--version] [--numeric-version] [--no-input]\n\
    \\n\
    \  CLI tool build with iris - a Haskell CLI framework\n\
    \\n\
    \Available options:\n\
    \  -h,--help                Show this help text\n\
    \  --version                Show application version\n\
    \  --numeric-version        Show only numeric application version\n\
    \  --no-input               Enter the terminal in non-interactive mode"

expectedNumericVersion :: String
expectedNumericVersion = "0.0.0.0"

cliSpec :: Spec
cliSpec = describe "Cli Options" $ do
    let parserPrefs  = Opt.defaultPrefs
    it "help without version environment" $ do
        let parserInfo = cmdParserInfo defaultCliEnvSettings
        let result = Opt.execParserPure parserPrefs parserInfo ["--help"]
        parseResultHandler result expectedHelpText
    it "help with version environment" $ do
        let cliEnvSettings = defaultCliEnvSettings { cliEnvSettingsVersionSettings = Just (defaultVersionSettings Autogen.version)}
        let parserInfo= cmdParserInfo cliEnvSettings
        let result = Opt.execParserPure parserPrefs parserInfo ["--help"]
        parseResultHandler result expectedHelpTextWithVersion
    it "--numeric-version returns correct version" $ do
        let cliEnvSettings = defaultCliEnvSettings { cliEnvSettingsVersionSettings = Just (defaultVersionSettings Autogen.version)}
        let parserInfo= cmdParserInfo cliEnvSettings
        let result = Opt.execParserPure parserPrefs parserInfo ["--numeric-version"]
        parseResultHandler result expectedNumericVersion
    it "CI interactivity check" $ do
        handleInteractiveMode NonInteractive `shouldReturn` NonInteractive
        isCi <- checkCI
        if isCi then handleInteractiveMode Interactive `shouldReturn` NonInteractive
        else handleInteractiveMode Interactive `shouldReturn` Interactive
    it "--version returns correct version text" $ do
        let expectedVersionMkDescription = ("Version " ++)
        let cliEnvSettings = defaultCliEnvSettings { cliEnvSettingsVersionSettings = Just $ (defaultVersionSettings Autogen.version) {versionSettingsMkDesc  = expectedVersionMkDescription}}
        let parserInfo= cmdParserInfo cliEnvSettings
        let expectedVersion = expectedVersionMkDescription expectedNumericVersion
        let result = Opt.execParserPure parserPrefs parserInfo ["--version"]
        parseResultHandler result expectedVersion
        where
            parseResultHandler parseResult expected =
                case parseResult of
                    -- The help functionality is baked into optparse-applicative and presents itself as a ParserFailure.
                    Opt.Failure (Opt.ParserFailure getFailure) -> do
                        let (helpText, _exitCode, _int) = getFailure "<iris-test>"
                        show helpText `shouldBe` expected
                    Opt.Success _ -> expectationFailure "Expected 'Failure' but got Success "
                    Opt.CompletionInvoked completionResult -> expectationFailure $ "Expected 'Failure' but got: " <> show completionResult
