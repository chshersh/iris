module Main (main) where

import Control.Monad.IO.Class (MonadIO (..))

import qualified Iris
import qualified Paths_iris as Autogen


newtype App a = App
    { unApp :: Iris.CliApp () () a
    } deriving newtype (Functor, Applicative, Monad, MonadIO)

appSettings :: Iris.CliEnvSettings () ()
appSettings = Iris.defaultCliEnvSettings
    { Iris.cliEnvSettingsHeaderDesc      = "Iris usage example"
    , Iris.cliEnvSettingsProgDesc        = "A simple grep utility"
    , Iris.cliEnvSettingsVersionSettings =
        Just (Iris.defaultVersionSettings Autogen.version)
            { Iris.versionSettingsMkDesc = \v -> "Iris Example v" <> v
            }
    }

app :: App ()
app = liftIO $ putStrLn "Hello from an Iris app!"

main :: IO ()
main = Iris.runCliApp appSettings $ unApp app
