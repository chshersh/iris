module Main (main) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)

import qualified Colourista
import qualified Iris
import qualified Paths_iris as Autogen


newtype App a = App
    { unApp :: Iris.CliApp () () a
    } deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader (Iris.CliEnv () ())
        )

appSettings :: Iris.CliEnvSettings () ()
appSettings = Iris.defaultCliEnvSettings
    { Iris.cliEnvSettingsHeaderDesc = "Iris usage example"
    , Iris.cliEnvSettingsProgDesc = "A simple grep utility"
    , Iris.cliEnvSettingsVersionSettings =
        Just (Iris.defaultVersionSettings Autogen.version)
            { Iris.versionSettingsMkDesc = \v -> "Iris Example v" <> v
            }
    }

app :: App ()
app = do
    liftIO $ putStrLn "Hello from an Iris app!"

    Iris.putStdoutColouredLn
        (Colourista.formatWith [Colourista.yellow, Colourista.bold])
        "I'm yellow"

    Iris.putStderrColouredLn
        (Colourista.formatWith [Colourista.blueBg])
        "I'm white on blue"

main :: IO ()
main = Iris.runCliApp appSettings $ unApp app
