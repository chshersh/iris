module Test.Iris.Cli.Interactive (cliInteractiveSpec) where

import           Options.Applicative  (getParseResult)
import           Test.Hspec           (Spec, describe, it, shouldBe,
                                       shouldReturn)

import           Iris.Cli.Colour      (ColourOption (..))
import           Iris.Cli.Interactive (InteractiveMode (..),
                                       handleInteractiveMode)
import           Iris.Cli.Internal
import           Iris.Cli.ParserInfo  (cmdParserInfo)
import           Iris.Settings        (defaultCliEnvSettings)

import           Test.Iris.Common     (checkCI)

import qualified Options.Applicative  as Opt

cliInteractiveSpec :: Spec
cliInteractiveSpec = describe "Cli Interactive Options" $ do
    let parserPrefs = Opt.defaultPrefs
    it "* Interactivity check" $ do
        handleInteractiveMode NonInteractive `shouldReturn` NonInteractive
        isCi <- checkCI
        if isCi
            then handleInteractiveMode Interactive `shouldReturn` NonInteractive
            else handleInteractiveMode Interactive `shouldReturn` Interactive
    it "* Handles colour mode" $ do
        let parserInfo = cmdParserInfo defaultCliEnvSettings
        let coloption args = getParseResult $ cmdColourOption <$> Opt.execParserPure parserPrefs parserInfo args
        coloption ["--colour"] `shouldBe` pure Always
        coloption ["--no-colour"] `shouldBe` pure Never
        coloption [] `shouldBe` pure Auto
