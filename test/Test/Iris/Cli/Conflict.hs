module Test.Iris.Cli.Conflict (cliConflictsSpec) where

import           Options.Applicative  (getParseResult)
import           Test.Hspec           (Expectation, Spec, describe,
                                       expectationFailure, it, shouldBe,
                                       shouldReturn)

import           Iris                 (CliEnvSettings (..))
import           Iris.Cli             (VersionSettings (versionSettingsMkDesc))
import           Iris.Cli.Colour      (ColourOption (..))
import           Iris.Cli.Interactive (InteractiveMode (..),
                                       handleInteractiveMode)
import           Iris.Cli.Internal
import           Iris.Cli.ParserInfo  (cmdParserInfo)
import           Iris.Cli.Version     (defaultVersionSettings)
import           Iris.Settings        (defaultCliEnvSettings)

import           Test.Iris.Common     (checkCI)

import qualified Options.Applicative  as Opt
import qualified Paths_iris           as Autogen

newtype UserDefinedParser a = UserDefinedParser {noInteractive :: a}

userDefinedNoInputOption :: Opt.Parser (UserDefinedParser String)
userDefinedNoInputOption =
    UserDefinedParser
        <$> Opt.strOption (Opt.long "no-input")

userDefinedNoInputSwitch :: Opt.Parser (UserDefinedParser Bool)
userDefinedNoInputSwitch =
    UserDefinedParser
        <$> Opt.switch (Opt.long "no-input")

userDefinedNoInputOnCommand :: Opt.Parser (UserDefinedParser Bool)
userDefinedNoInputOnCommand =
    Opt.subparser
        ( Opt.command
            "test-command"
            (Opt.info userDefinedNoInputSwitch Opt.fullDesc)
        )

customParserSettings :: Opt.Parser (UserDefinedParser a) -> CliEnvSettings (UserDefinedParser a) ()
customParserSettings parser =
    CliEnvSettings
        { cliEnvSettingsCmdParser = parser
        , cliEnvSettingsAppEnv = ()
        , cliEnvSettingsHeaderDesc = "Simple CLI program"
        , cliEnvSettingsProgDesc = "CLI tool build with iris - a Haskell CLI framework"
        , cliEnvSettingsVersionSettings = Nothing
        , cliEnvSettingsAppName = Nothing
        }

argValue :: String
argValue = "someValue"

expectedErrorTextUserDefinedNoInputArg :: String
expectedErrorTextUserDefinedNoInputArg =
    "Invalid argument `"
        <> argValue
        <> "'\n\
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

cliConflictsSpec :: Spec
cliConflictsSpec = describe "Cli Parser Conflicts" $ do
    let parserPrefs = Opt.defaultPrefs
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
        let result = Opt.execParserPure parserPrefs parserInfo ["--no-input", "test-command"]
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
