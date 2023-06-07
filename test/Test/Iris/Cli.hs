module Test.Iris.Cli (cliSpec) where

import           Test.Hspec                (Spec, describe)

import           Test.Iris.Cli.Conflict    (cliConflictsSpec)
import           Test.Iris.Cli.Help        (cliHelpSpec)
import           Test.Iris.Cli.Interactive (cliInteractiveSpec)
import           Test.Iris.Cli.Version     (cliVersionSpec)


cliSpec :: Spec
cliSpec = describe "Cli Options" $ do
    cliHelpSpec
    cliVersionSpec
    cliInteractiveSpec
    cliConflictsSpec
