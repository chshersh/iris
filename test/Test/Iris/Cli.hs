{-# LANGUAGE ScopedTypeVariables #-}
module Test.Iris.Cli (cliSpec, cliSpecParserConflicts) where

import Test.Hspec (Spec, describe, it, shouldBe, expectationFailure, shouldReturn, Expectation)

import Iris.Cli.ParserInfo (cmdParserInfo)
import Iris.Settings (CliEnvSettings(..), defaultCliEnvSettings)
import Iris.Cli.Version (defaultVersionSettings)
import Iris.Cli.Internal
import qualified Options.Applicative as Opt
import qualified Paths_iris as Autogen
import Iris.Cli (VersionSettings(versionSettingsMkDesc))
import Iris.Cli.Interactive (handleInteractiveMode, InteractiveMode (..))
import System.Environment (lookupEnv)


checkCI :: IO Bool
checkCI = (== Just "true") <$> lookupEnv "CI"

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
        parseResultHandlerFailure result expectedHelpText
    it "help with version environment" $ do
        let cliEnvSettings = defaultCliEnvSettings { cliEnvSettingsVersionSettings = Just (defaultVersionSettings Autogen.version)}
        let parserInfo = cmdParserInfo cliEnvSettings
        let result = Opt.execParserPure parserPrefs parserInfo ["--help"]
        parseResultHandlerFailure result expectedHelpTextWithVersion
    it "--numeric-version returns correct version" $ do
        let cliEnvSettings = defaultCliEnvSettings { cliEnvSettingsVersionSettings = Just (defaultVersionSettings Autogen.version)}
        let parserInfo = cmdParserInfo cliEnvSettings
        let result = Opt.execParserPure parserPrefs parserInfo ["--numeric-version"]
        parseResultHandlerFailure result expectedNumericVersion
    it "CI interactivity check" $ do
        handleInteractiveMode NonInteractive `shouldReturn` NonInteractive
        isCi <- checkCI
        if isCi then handleInteractiveMode Interactive `shouldReturn` NonInteractive
        else handleInteractiveMode Interactive `shouldReturn` Interactive
    it "--version returns correct version text" $ do
        let expectedVersionMkDescription = ("Version " ++)
        let cliEnvSettings = defaultCliEnvSettings { cliEnvSettingsVersionSettings = Just $ (defaultVersionSettings Autogen.version) {versionSettingsMkDesc  = expectedVersionMkDescription}}
        let parserInfo = cmdParserInfo cliEnvSettings
        let expectedVersion = expectedVersionMkDescription expectedNumericVersion
        let result = Opt.execParserPure parserPrefs parserInfo ["--version"]
        parseResultHandlerFailure result expectedVersion

newtype UserDefinedParser a
  = UserDefinedParser { noInteractive :: a }

userDefinedNoInputOption :: Opt.Parser (UserDefinedParser String)
userDefinedNoInputOption = UserDefinedParser
      <$> Opt.strOption (Opt.long "no-input")

userDefinedNoInputSwitch :: Opt.Parser (UserDefinedParser Bool)
userDefinedNoInputSwitch = UserDefinedParser
      <$> Opt.switch (Opt.long "no-input")

userDefinedNoInputOnCommand :: Opt.Parser (UserDefinedParser Bool)
userDefinedNoInputOnCommand = Opt.subparser
       ( Opt.command "test-command"
         (Opt.info userDefinedNoInputSwitch Opt.fullDesc))

customParserSettings :: Opt.Parser (UserDefinedParser a) -> CliEnvSettings (UserDefinedParser a) ()
customParserSettings parser = CliEnvSettings
    { cliEnvSettingsCmdParser       = parser
    , cliEnvSettingsAppEnv          = ()
    , cliEnvSettingsHeaderDesc      = "Simple CLI program"
    , cliEnvSettingsProgDesc        = "CLI tool build with iris - a Haskell CLI framework"
    , cliEnvSettingsVersionSettings = Nothing
    , cliEnvSettingsRequiredTools   = []
    }

argValue :: String
argValue = "someValue"

expectedErrorTextUserDefinedNoInputArg :: String
expectedErrorTextUserDefinedNoInputArg =
  "Invalid argument `" <> argValue <> "'\n\
  \\n\
  \Usage: <iris-test> [--no-input] --no-input ARG\n\
  \\n\
  \  CLI tool build with iris - a Haskell CLI framework"

expectedErrorTextUserDefinedNoInputNoArg :: String
expectedErrorTextUserDefinedNoInputNoArg =
  "Missing: --no-input ARG\n\
  \\n\
  \Usage: <iris-test> [--no-input] --no-input ARG\n\
  \\n\
  \  CLI tool build with iris - a Haskell CLI framework"
         
cliSpecParserConflicts :: Spec
cliSpecParserConflicts = describe "Cli Parser Conflicts" $ do
    let parserPrefs  = Opt.defaultPrefs
    it "--no-input=someValue defined by user - arg provided" $ do
        let parserInfo = cmdParserInfo $ customParserSettings userDefinedNoInputOption
        let result = Opt.execParserPure parserPrefs parserInfo ["--no-input", argValue]
        parseResultHandlerFailure result expectedErrorTextUserDefinedNoInputArg
    it "--no-input=someValue defined by user - no arg provided" $ do
        let parserInfo = cmdParserInfo $ customParserSettings userDefinedNoInputOption
        let result = Opt.execParserPure parserPrefs parserInfo ["--no-input"]
        parseResultHandlerFailure result expectedErrorTextUserDefinedNoInputNoArg
    it "--no-input=someValue defined by user - not provided at all" $ do
        let parserInfo = cmdParserInfo $ customParserSettings userDefinedNoInputOption
        let result = Opt.execParserPure parserPrefs parserInfo []
        parseResultHandlerFailure result expectedErrorTextUserDefinedNoInputNoArg
    it "--no-input switch defined by user - provided" $ do
        let parserInfo = cmdParserInfo $ customParserSettings userDefinedNoInputSwitch
        let result = Opt.execParserPure parserPrefs parserInfo ["--no-input"]
        parseResultHandlerSuccess result ["NonInteractive", "False"]
    it "--no-input switch defined by user - not provided" $ do
        let parserInfo = cmdParserInfo $ customParserSettings userDefinedNoInputSwitch
        let result = Opt.execParserPure parserPrefs parserInfo []
        parseResultHandlerSuccess result ["Interactive", "False"]
    it "--no-input switch with command defined by user - user provided" $ do
        let parserInfo = cmdParserInfo $ customParserSettings userDefinedNoInputOnCommand
        let result = Opt.execParserPure parserPrefs parserInfo ["test-command", "--no-input"]
        parseResultHandlerSuccess result ["Interactive", "True"]
    it "--no-input switch with command defined by user - internal provided" $ do
        let parserInfo = cmdParserInfo $ customParserSettings userDefinedNoInputOnCommand
        let result = Opt.execParserPure parserPrefs parserInfo ["--no-input","test-command"]
        parseResultHandlerSuccess result ["NonInteractive", "False"]
     
parseResultHandlerSuccess :: Show b => Opt.ParserResult (Cmd (UserDefinedParser b)) -> [String] -> Expectation
parseResultHandlerSuccess parseResult expected =
    case parseResult of
        Opt.Failure _ -> expectationFailure "Expected 'Success' but got 'Failure' "
        Opt.Success a -> do
            let internalNoInput = show $ cmdInteractiveMode a
            let userDefinedNoInput = show . noInteractive . cmdCmd $ a
            shouldBe [internalNoInput, userDefinedNoInput] expected
        Opt.CompletionInvoked completionResult -> expectationFailure $ "Expected 'Success' but got: " <> show completionResult
        
parseResultHandlerFailure :: Opt.ParserResult a -> String -> Expectation
parseResultHandlerFailure parseResult expected =
    case parseResult of
        -- The help functionality is baked into optparse-applicative and presents itself as a ParserFailure.
        Opt.Failure (Opt.ParserFailure getFailure) -> do
            let (helpText, _exitCode, _int) = getFailure "<iris-test>"
            show helpText `shouldBe` expected
        Opt.Success _ -> expectationFailure "Expected 'Failure' but got 'Success' "
        Opt.CompletionInvoked completionResult -> expectationFailure $ "Expected 'Failure' but got: " <> show completionResult