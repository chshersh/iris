There is a basic example usage of Iris and a tutorial-like explanation:

First of all, let's create a newtype wrapper for the main monad of Iris:

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

It uses `Iris.CliApp SimpleGrep () a` that means `CliApp` will consume `SimpleGrep` data of commands for CLI description. Also, there's `()` in the second argument, it could be a specific environment type for our application, but in our case, it isn't necessary.
`SimpleGrep` is described below:

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

Finally, there's some little boilerplate code for toy `grep` reimplementation, see the remaining part of the example:

```haskell

occurences :: [T.Text] -> T.Text -> [(Int, T.Text)]
occurences lines substring = go lines 0
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

main :: IO ()
main = Iris.runCliApp appSettings $ unApp app
```