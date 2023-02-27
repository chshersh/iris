module Test.Iris.Colour.Mode (modeSpec) where

import Data.Foldable (for_)
import System.Environment (setEnv, unsetEnv)
import System.IO (stderr, stdout)
import Test.Hspec (Spec, before_, describe, it, shouldReturn)

import Iris.Cli.Colour (ColourOption (..))
import Iris.Colour.Mode (ColourMode (..), detectColourMode)

import Test.Iris.Common (checkCI)

modeSpec :: Spec
modeSpec = before_ clearAppEnv $ describe "Mode" $ do
    let detectStdoutColour option = detectColourMode stdout option (Just "myapp")
    let detectStderrColour option = detectColourMode stderr option (Just "myapp")

    it "DisableColour when --no-colour" $ do
        detectStdoutColour Never `shouldReturn` DisableColour
        detectStderrColour Never `shouldReturn` DisableColour

    it "EnableColour when --colour" $ do
        detectStdoutColour Always `shouldReturn` EnableColour
        detectStderrColour Always `shouldReturn` EnableColour

    it "EnableColour in clear environment" $ do
        ciColour <- colourWithCI
        detectStdoutColour Auto `shouldReturn` ciColour
        detectStderrColour Auto `shouldReturn` ciColour

    it "DisableColour when NO_COLOR is set" $ do
        setEnv "NO_COLOR" "1"
        detectStdoutColour Auto `shouldReturn` DisableColour
        detectStderrColour Auto `shouldReturn` DisableColour

    it "DisableColour when NO_COLOUR is set" $ do
        setEnv "NO_COLOUR" "1"
        detectStdoutColour Auto `shouldReturn` DisableColour
        detectStderrColour Auto `shouldReturn` DisableColour

    it "DisableColour when MYAPP_NO_COLOR is set" $ do
        setEnv "MYAPP_NO_COLOR" "1"
        detectStdoutColour Auto `shouldReturn` DisableColour
        detectStderrColour Auto `shouldReturn` DisableColour

    it "DisableColour when MYAPP_NO_COLOUR is set" $ do
        setEnv "MYAPP_NO_COLOUR" "1"
        detectStdoutColour Auto `shouldReturn` DisableColour
        detectStderrColour Auto `shouldReturn` DisableColour

    it "DisableColour when TERM=dumb" $ do
        setEnv "TERM" "dumb"
        detectStdoutColour Auto `shouldReturn` DisableColour
        detectStderrColour Auto `shouldReturn` DisableColour

    it "EnableColour when TERM=xterm-256color" $ do
        setEnv "TERM" "xterm-256color"
        ciColour <- colourWithCI
        detectStdoutColour Auto `shouldReturn` ciColour
        detectStderrColour Auto `shouldReturn` ciColour

    it "DisableColour when CI is set" $ do
        ciColour <- colourWithCI
        detectStdoutColour Auto `shouldReturn` ciColour
        detectStderrColour Auto `shouldReturn` ciColour

-- Helper functions

testEnvVars :: [String]
testEnvVars =
    [ "NO_COLOR"
    , "NO_COLOUR"
    , "MYAPP_NO_COLOR"
    , "MYAPP_NO_COLOUR"
    , "TERM"
    ]

clearAppEnv :: IO ()
clearAppEnv = for_ testEnvVars unsetEnv

colourWithCI :: IO ColourMode
colourWithCI = do
    isCi <- checkCI
    pure $ if isCi then DisableColour else EnableColour
