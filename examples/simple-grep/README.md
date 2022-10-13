# Introduction

There is a basic example usage of Iris and a tutorial-like explanation:

This example can be executed via the below command:

```shell
cabal exec simple-grep -- -f DIR -s TARGET_WORD
```

## Preamble: imports and language extensions

First of all, let's define imports and extensions at the head of the tutorial:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}


module Main (main) where

import Prelude hiding (readFile)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Options.Applicative as Opt

import qualified Colourista
import qualified Iris
import qualified Paths_iris as Autogen
```

So, we're writing a simple grep utility, we need here `Colourista` library for printing colored messages via the Iris app. Other libraries such as `Text`, `ByteString` is standard for our specific task. `optparse-applicative` is needed here for defining the set of commands that Iris will consume.

## Core data types

After imports, let's define our main monad and derive another helper classes:

```haskell
newtype App a = App
    { unApp :: Iris.CliApp SimpleGrep () a
    } deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader (Iris.CliEnv SimpleGrep ())
        )
```

`Iris.CliApp` here is the main monad of the library. This monad will consume our datatype of commands `SimpleGrep` in his first argument and in the second argument - application env datatype that might be empty depending on your wishes and goals. However, we can get application datatype by using `ask` or `Iris.asksCliEnv` in our `App` monad. Command datatype, after consuming, will be parsed and putted into `--help` list.

Here's our commands for utility:

```haskell
data SimpleGrep = SimpleGrep
    {
      filePath :: !String
    , substring :: !String
    }
```

After the creation of a base monad with data-type of commands, let's instantiate settings to Iris:

```haskell
appSettings :: Iris.CliEnvSettings SimpleGrep ()
appSettings = Iris.defaultCliEnvSettings
    { Iris.cliEnvSettingsHeaderDesc = "Iris usage example"
    , Iris.cliEnvSettingsProgDesc = "A simple grep utility"
    , Iris.cliEnvSettingsVersionSettings =
        Just (Iris.defaultVersionSettings Autogen.version)
            { Iris.versionSettingsMkDesc = \v -> "Simple grep utility v" <> v
            }
    , Iris.cliEnvSettingsRequiredTools = []

    , Iris.cliEnvSettingsCmdParser = SimpleGrep
        <$> Opt.strOption
            (  Opt.long "filePath"
            <> Opt.short 'f'
            <> Opt.metavar "PATH"
            <> Opt.help "filePath to find and grep file"
            )
        <*> Opt.strOption
            (  Opt.long "substring"
            <> Opt.short 's'
            <> Opt.help "Substring to find and highlight"
            )
    }
```

We describe settings for the app based on `defaultCliEnvSettings`. It helps when we don't need some option to describe, so we can just skip it. Here we can write a CLI-program description, put required tools with a list and then pass commands with the `Parser a` type from `optparse-applicative` to the `cliEnvSettingsCmdParser` field.

Important to note, Iris has `Iris.cliEnvSettingsRequiredTools` property for checking that system has those tools, if not, it throws `CliEnvException` exception.

# Main monad's do-calculation

Finally, after setting up the configuration, we can describe our computation of CLI:

```haskell
app :: App ()
app = do
    SimpleGrep 
        { filePath, substring } <- Iris.asksCliEnv Iris.cliEnvCmd

    formattedPrinter "Starting grepping 🔥" Colourista.white
    file <- liftIO $ BSL.readFile filePath

    let fileName = "file name: " `T.append` (last $ T.split (== '/') $ T.pack filePath)
    let linedFile = T.lines $ TLE.decodeUtf8 file
    let substringText = T.pack substring

    formattedPrinter fileName Colourista.cyan

    occurencesPrinter $ occurences linedFile substringText
        where
        formattedPrinter :: T.Text -> BS.ByteString -> App ()
        formattedPrinter txt colour =
            Iris.putStderrColouredLn
            (Colourista.formatWith [colour, Colourista.bold])
            $ "\n " `BS.append` (BSL.toStrict $ TLE.encodeUtf8 txt) `BS.append` " "
```

Our main function and other not-important boilerplate functions for grep:

```haskell
main :: IO ()
main = Iris.runCliApp appSettings $ unApp app


occurences :: [T.Text] -> T.Text -> [(Int, T.Text)]
occurences ocs substring = go ocs 0
    where
        go (x:xs) idx | substring `T.isInfixOf` x = (idx, x) : go xs (idx + 1)
                      | otherwise = go xs (idx + 1)
        go [] _ = []

occurencesPrinter :: [(Int, T.Text)] -> App ()
occurencesPrinter = mapM_ unpack
    where
        unpack :: (Int, T.Text) -> App ()
        unpack (idx, line) = do
            liftIO $ printIdx idx
            liftIO $ putStr ":"
            printLine line

        printIdx idx = putStr $ " " ++ show idx

        printLine :: T.Text -> App ()
        printLine x = Iris.putStdoutColouredLn
            (Colourista.formatWith [Colourista.yellow, Colourista.bold])
                $ BSL.toStrict $ TLE.encodeUtf8 x
```

So, we'd wish to execute all that stuff. Let's do it!

```shell
cabal exec simple-grep -- -f /some/dir/iris/iris.cabal -s iris
```

And output with occurences of "iris":

```
Starting grepping 🔥 

 file name: iris.cabal 
 1:name:                iris
 6:    See [README.md](https://github.com/chshersh/iris#iris) for more details.
 7:homepage:            https://github.com/chshersh/iris
 8:bug-reports:         https://github.com/chshersh/iris/issues
 25:  location:            https://github.com/chshersh/iris.git
 78:  build-depends:       , iris
 118:  autogen-modules:     Paths_iris
 119:  other-modules:       Paths_iris
 122:     , iris
 135:  autogen-modules:     Paths_iris
 136:  other-modules:       Paths_iris
 149:test-suite iris-test
 159:    Paths_iris
 163:    , iris
```