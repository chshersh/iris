module Test.Iris.Cli.Version (cliVersionSpec) where

import           Iris                (CliEnvSettings (..))
import           Iris.Cli            (VersionSettings (versionSettingsMkDesc))
import           Iris.Cli.ParserInfo (cmdParserInfo)
import           Iris.Cli.Version    (defaultVersionSettings)
import           Iris.Settings       (defaultCliEnvSettings)
import           Test.Hspec          (Expectation, Spec, describe,
                                      expectationFailure, it, shouldBe)

import qualified Options.Applicative as Opt
import qualified Paths_iris          as Autogen


expectedNumericVersion :: String
expectedNumericVersion = "0.1.0.0"

cliVersionSpec :: Spec
cliVersionSpec = describe "Cli Version Options" $ do
    let parserPrefs = Opt.defaultPrefs
    it "--version returns correct version text" $ do
        let expectedVersionMkDescription = ("Version " ++)
        let cliEnvSettings = defaultCliEnvSettings{cliEnvSettingsVersionSettings = Just $ (defaultVersionSettings Autogen.version){versionSettingsMkDesc = expectedVersionMkDescription}}
        let parserInfo = cmdParserInfo cliEnvSettings
        let expectedVersion = expectedVersionMkDescription expectedNumericVersion
        let result = Opt.execParserPure parserPrefs parserInfo ["--version"]
        parseResultHandlerFailure result expectedVersion
    it "--numeric-version returns correct version" $ do
        let cliEnvSettings = defaultCliEnvSettings{cliEnvSettingsVersionSettings = Just (defaultVersionSettings Autogen.version)}
        let parserInfo = cmdParserInfo cliEnvSettings
        let result = Opt.execParserPure parserPrefs parserInfo ["--numeric-version"]
        parseResultHandlerFailure result expectedNumericVersion

parseResultHandlerFailure :: Opt.ParserResult a -> String -> Expectation
parseResultHandlerFailure parseResult expected =
    case parseResult of
        -- The help functionality is baked into optparse-applicative and presents itself as a ParserFailure.
        Opt.Failure (Opt.ParserFailure getFailure) -> do
            let (helpText, _exitCode, _int) = getFailure "<iris-test>"
            show helpText `shouldBe` expected
        Opt.Success _ -> expectationFailure "Expected 'Failure' but got 'Success' "
        Opt.CompletionInvoked completionResult -> expectationFailure $ "Expected 'Failure' but got: " <> show completionResult
