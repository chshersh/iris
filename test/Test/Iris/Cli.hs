{-# LANGUAGE ScopedTypeVariables #-}
module Test.Iris.Cli (cliSpec, cliSpecParserConflicts) where

import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, shouldBe, shouldReturn)

import Iris (CliEnvSettings (..))
import Iris.Cli (VersionSettings (versionSettingsMkDesc))
import Iris.Cli.Interactive (InteractiveMode (..), handleInteractiveMode)
import Iris.Cli.Internal
import Iris.Cli.ParserInfo (cmdParserInfo)
import Iris.Cli.TripleOption (TripleOption (..))
import Iris.Cli.Version (defaultVersionSettings)
import Iris.Colour.Detect (detectColourDisabled)
import Iris.Colour.Mode (ColourMode (..), actualHandleColourMode)
import Iris.Settings (defaultCliEnvSettings)
import Options.Applicative (getParseResult)
import qualified Options.Applicative as Opt
import qualified Paths_iris as Autogen
import System.Environment (lookupEnv, setEnv)
import System.IO (stdout)


checkCI :: IO Bool
checkCI = (== Just "true") <$> lookupEnv "CI"

expectedHelpText :: String
expectedHelpText =
    "Simple CLI program\n\
    \\n\
    \Usage: <iris-test> [--no-input] [--colour | --no-colour]\n\
    \\n\
    \  CLI tool build with iris - a Haskell CLI framework\n\
    \\n\
    \Available options:\n\
    \  -h,--help                Show this help text\n\
    \  --no-input               Enter the terminal in non-interactive mode\n\
    \  --colour                 Enable colours\n\
    \  --no-colour              Disable colours"

expectedHelpTextWithVersion :: String
expectedHelpTextWithVersion =
    "Simple CLI program\n\
    \\n\
    \Usage: <iris-test> [--version] [--numeric-version] [--no-input] \n\
    \                   [--colour | --no-colour]\n\
    \\n\
    \  CLI tool build with iris - a Haskell CLI framework\n\
    \\n\
    \Available options:\n\
    \  -h,--help                Show this help text\n\
    \  --version                Show application version\n\
    \  --numeric-version        Show only numeric application version\n\
    \  --no-input               Enter the terminal in non-interactive mode\n\
    \  --colour                 Enable colours\n\
    \  --no-colour              Disable colours"

expectedNumericVersion :: String
expectedNumericVersion = "0.0.0.0"

clearAppEnv :: IO()
clearAppEnv = mconcat $ setEnv <$> ["NO_COLOR","NO_COLOUR","MYAPP_NO_COLOR","MYAPP_NO_COLOUR","TERM"] <*> [""]

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
    it "Handles colour mode" $ do
        let parserInfo = cmdParserInfo defaultCliEnvSettings
        let coloption args = getParseResult $ cmdColourOption <$> Opt.execParserPure parserPrefs parserInfo args
        coloption ["--colour"] `shouldBe` pure TOAlways
        coloption ["--no-colour"] `shouldBe` pure TONever
        coloption [] `shouldBe` pure TOAuto
    it "Applies base nocolour environment" $ do
        clearAppEnv
        detectColourDisabled (Just "MYAPP") `shouldReturn` False
        setEnv "NO_COLOR" "TRUE"
        detectColourDisabled (Just "MYAPP") `shouldReturn` True
        detectColourDisabled Nothing `shouldReturn` True
        clearAppEnv
        setEnv "NO_COLOUR" "TRUE"
        detectColourDisabled (Just "MYAPP") `shouldReturn` True
        detectColourDisabled Nothing `shouldReturn` True
    it "Applies app specific nocolour environment" $ do
        clearAppEnv
        detectColourDisabled (Just "MYAPP") `shouldReturn` False
        setEnv "MYAPP_NO_COLOR" "TRUE"
        detectColourDisabled (Just "MYAPP") `shouldReturn` True
        detectColourDisabled Nothing `shouldReturn` False
        clearAppEnv
        setEnv "MYAPP_NO_COLOUR" "TRUE"
        detectColourDisabled (Just "MYAPP") `shouldReturn` True
        detectColourDisabled Nothing `shouldReturn` False
    it "Disables colour on dumb terminals" $ do
        clearAppEnv
        setEnv "TERM" "NOTDUMB"
        detectColourDisabled (Just "MYAPP") `shouldReturn` False
        detectColourDisabled Nothing `shouldReturn` False
        setEnv "TERM" "dumb"
        detectColourDisabled (Just "MYAPP") `shouldReturn` True
        detectColourDisabled Nothing `shouldReturn` True
        setEnv "NO_COLOR" "TRUE"
        setEnv "MYAPP_NO_COLOR" "TRUE"
        detectColourDisabled (Just "MYAPP") `shouldReturn` True
        detectColourDisabled Nothing `shouldReturn` True
    it "CI colour check" $ do
        isCi <- checkCI
        clearAppEnv
        let ciColour = if isCi then DisableColour else EnableColour
        actualHandleColourMode (Just "MYAPP") TONever stdout `shouldReturn` DisableColour
        actualHandleColourMode (Just "MYAPP") TOAlways stdout `shouldReturn` ciColour
        actualHandleColourMode (Just "MYAPP") TOAuto stdout `shouldReturn` ciColour
        setEnv "NO_COLOUR" "TRUE"
        actualHandleColourMode (Just "MYAPP") TOAuto stdout `shouldReturn` DisableColour
        actualHandleColourMode (Just "MYAPP") TOAlways stdout `shouldReturn` ciColour
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
    }

argValue :: String
argValue = "someValue"

expectedErrorTextUserDefinedNoInputArg :: String
expectedErrorTextUserDefinedNoInputArg =
  "Invalid argument `" <> argValue <> "'\n\
  \\n\
  \Usage: <iris-test> [--no-input] --no-input ARG [--colour | --no-colour]\n\
  \\n\
  \  CLI tool build with iris - a Haskell CLI framework"

expectedErrorTextUserDefinedNoInputNoArg :: String
expectedErrorTextUserDefinedNoInputNoArg =
  "Missing: --no-input ARG\n\
  \\n\
  \Usage: <iris-test> [--no-input] --no-input ARG [--colour | --no-colour]\n\
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
