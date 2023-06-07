module Test.Iris.Cli.Help (cliHelpSpec) where

import           Test.Hspec          (Expectation, Spec, describe,
                                      expectationFailure, it, shouldBe)

import           Iris                (CliEnvSettings (..))
import           Iris.Cli.ParserInfo (cmdParserInfo)
import           Iris.Cli.Version    (defaultVersionSettings)
import           Iris.Settings       (defaultCliEnvSettings)

import qualified Options.Applicative as Opt
import qualified Paths_iris          as Autogen

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
    \  --colour                 Always output colours\n\
    \  --no-colour              Never output colours"

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
    \  --colour                 Always output colours\n\
    \  --no-colour              Never output colours"

cliHelpSpec :: Spec
cliHelpSpec = describe "Cli Help Options" $ do
    let parserPrefs = Opt.defaultPrefs
    it "--help without version environment" $ do
        let parserInfo = cmdParserInfo defaultCliEnvSettings
        let result = Opt.execParserPure parserPrefs parserInfo ["--help"]
        parseResultHandlerFailure result expectedHelpText
    it "--help with version environment" $ do
        let cliEnvSettings = defaultCliEnvSettings{cliEnvSettingsVersionSettings = Just (defaultVersionSettings Autogen.version)}
        let parserInfo = cmdParserInfo cliEnvSettings
        let result = Opt.execParserPure parserPrefs parserInfo ["--help"]
        parseResultHandlerFailure result expectedHelpTextWithVersion

parseResultHandlerFailure :: Opt.ParserResult a -> String -> Expectation
parseResultHandlerFailure parseResult expected =
    case parseResult of
        -- The help functionality is baked into optparse-applicative and presents itself as a ParserFailure.
        Opt.Failure (Opt.ParserFailure getFailure) -> do
            let (helpText, _exitCode, _int) = getFailure "<iris-test>"
            show helpText `shouldBe` expected
        Opt.Success _ -> expectationFailure "Expected 'Failure' but got 'Success' "
        Opt.CompletionInvoked completionResult -> expectationFailure $ "Expected 'Failure' but got: " <> show completionResult
