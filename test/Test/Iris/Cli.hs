{-# LANGUAGE ScopedTypeVariables #-}
module Test.Iris.Cli (cliSpec) where

import Test.Hspec (Spec, describe, it)

import Iris.Cli.ParserInfo (cmdParserInfo)
import Iris.Settings (defaultCliEnvSettings, cliEnvSettingsVersionSettings)
import Iris.Cli.Cmd (Cmd (..))
import Iris.Cli.Version (defaultVersionSettings)
import qualified Options.Applicative as Opt
import Data.List (isInfixOf)
import qualified Paths_iris as Autogen



type HelpOptions = [String]

cliSpec :: Spec
cliSpec = describe "Cli Options" $ do
    let parserPrefs :: Opt.ParserPrefs = Opt.defaultPrefs 
    it "help without version environment should output the options help and no-input and should not output the options version and numeric-version" $ do
        let parserInfo :: Opt.ParserInfo (Cmd ()) = cmdParserInfo defaultCliEnvSettings
        let result :: Opt.ParserResult (Cmd ()) = Opt.execParserPure parserPrefs parserInfo ["--help"]
        validHelp result (\text -> helpHasOptions text ["--help", "--no-input"] && not (helpHasOptions text ["--version", "--numeric-version"]))
    it "help with version environment should output help, no-input options, version and numeric-version" $ do
        let cliEnvSettings = defaultCliEnvSettings { cliEnvSettingsVersionSettings = Just (defaultVersionSettings Autogen.version)}
        let parserInfo :: Opt.ParserInfo  (Cmd ()) = cmdParserInfo cliEnvSettings
        let result :: Opt.ParserResult (Cmd ()) = Opt.execParserPure parserPrefs parserInfo ["--help"]
        validHelp result (\text -> helpHasOptions text ["--help", "--no-input", "--version", "--numeric-version"])
        where
            validHelp :: Opt.ParserResult (Cmd ()) -> (String -> Bool) -> Bool
            validHelp result f =
                -- The help functionality is baked into optparse-applicative and presents itself as a ParserFailure.
                case result of
                    Opt.Failure text -> f (show text)
                    _ -> False
            helpHasOptions :: String -> HelpOptions -> Bool
            helpHasOptions text options = all (\x -> x `isInfixOf` show text) options
