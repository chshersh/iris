{-# LANGUAGE ScopedTypeVariables #-}
module Test.Iris.Cli (cliSpec) where

import Test.Hspec (Spec, describe, it, shouldReturn, shouldSatisfy)

import Iris.Env (mkCliEnv, CliEnv)
import Iris.Cli.ParserInfo (cmdParserInfo)
import Iris.Settings (defaultCliEnvSettings, cliEnvSettingsCmdParser, Cmd(..))
import qualified Options.Applicative as Opt
import qualified Options.Applicative.Types as OptTypes

cliSpec :: Spec
cliSpec = describe "Cli" $ do
    it "help should output default options" $ do
        env :: Iris.Env.CliEnv () () <- mkCliEnv defaultCliEnvSettings
        let parserPrefs :: Opt.ParserPrefs = Opt.ParserPrefs "suffix" False False False OptTypes.Backtrack 80 False False 0
        let parserInfo :: Opt.ParserInfo (Cmd ()) = cmdParserInfo defaultCliEnvSettings
        -- include getParseResult
        -- https://hackage.haskell.org/package/optparse-applicative-0.14.3.0/docs/Options-Applicative-Extra.html#v:getParseResult
        let result :: Opt.ParserResult (Cmd ()) = Opt.execParserPure parserPrefs parserInfo ["--help"]
        case result of
            Opt.Success a -> error "Success case"
            Opt.Failure b -> error $ mconcat ["Failure case: {", show b, "}"]
            Opt.CompletionInvoked c -> error $ show c	
        return False `shouldReturn` True
        --let b :: _ = cmdParserInfo a
        --result :: _ <- Opt.execParserPure a (b a) ["--help"] 
        --return env `shouldReturn` ""
    