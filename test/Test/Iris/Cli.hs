{-# LANGUAGE ScopedTypeVariables #-}
module Test.Iris.Cli (cliSpec) where

import Test.Hspec (Spec, describe, it, shouldReturn, shouldSatisfy)

import Iris.Env (mkCliEnv, defaultCliEnvSettings, CliEnv, cliEnvSettingsCmdParser, cmdParserInfo2)
import qualified Options.Applicative as Opt

cliSpec :: Spec
cliSpec = describe "Cli" $ do
    it "help should output default options" $ do
        env :: Iris.Env.CliEnv () () <- mkCliEnv defaultCliEnvSettings
        let a :: Opt.ParserPrefs = cliEnvSettingsCmdParser env
        let b :: _ = cmdParserInfo2 a
        result :: _ <- Opt.execParserPure a (b a) ["--help"] 
        return env `shouldReturn` ""
    