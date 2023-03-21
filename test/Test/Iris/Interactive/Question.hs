module Test.Iris.Interactive.Question (questionSpec) where

import Test.Hspec (Spec, describe, it, shouldReturn)

import Control.Monad (forM_)
import Data.Text (unpack)

import Iris.Interactive.Question (
    -- under test
    YesNo (..),
    parseYesNo,
 )

questionSpec :: Spec
questionSpec =
    describe "Question - parse YesNo" $ do
        describe "should parse to Just Yes" $ do
            forM_ ["y", "Y", "YES", "yes", "Yes"] $
                \m -> do
                    it (unpack m) $ do
                        pure (parseYesNo m) `shouldReturn` Just Yes

        describe "should parse to Just No" $ do
            forM_ ["n", "N", "NO", "no", "No"] $
                \m -> do
                    it (unpack m) $ do
                        pure (parseYesNo m) `shouldReturn` Just No

        describe "should parse to Nothing" $ do
            forM_ ["a", "ye", "NOone", "yesterday", "oui"] $
                \m -> do
                    it (unpack m) $ do
                        pure (parseYesNo m) `shouldReturn` Nothing

            it "[empty string]" $ do
                pure (parseYesNo "") `shouldReturn` Nothing
