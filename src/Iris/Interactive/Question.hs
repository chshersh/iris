{-# LANGUAGE FlexibleContexts #-}

{- |
Module                  : Iris.Interactive.Question
Copyright               : (c) 2023 Dmitrii Kovanikov
SPDX-License-Identifier : MPL-2.0
Maintainer              : Dmitrii Kovanikov <kovanikov@gmail.com>
Stability               : Experimental
Portability             : Portable

Asking Questions. Receiving answers.

@since x.x.x.x
-}

module Iris.Interactive.Question (
    yesno,
    YesNo (..),
    parseYesNo, -- export needed for testing 
) where 

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)

import Data.Text (Text, toUpper)
import System.IO (hFlush, stdout)
import qualified Data.Text.IO as TIO

import Iris.Cli.Interactive (InteractiveMode (..))
import Iris.Env (CliEnv (..), asksCliEnv)

parseYesNo :: Text -> Maybe YesNo
parseYesNo t = case toUpper t of
    "Y" -> Just Yes
    "YES" -> Just Yes
    "N" -> Just No
    "NO" -> Just No
    _ -> Nothing

{- | Parsed as Yes: "Y", "YES" (lower- or uppercase)

Parsed as No: "N", "NO" (lower- or uppercase)
-}
data YesNo = 
    No | Yes
    deriving stock
        ( Show
          -- ^ @since x.x.x.x
        , Eq
          -- ^ @since x.x.x.x
        , Ord
          -- ^ @since x.x.x.x
        , Enum
          -- ^ @since x.x.x.x
        , Bounded
          -- ^ @since x.x.x.x
        )

{- | Ask a yes/no question to stdout, read the reply from terminal, return an Answer.  
     
In case of running non-interactively, return the provided default 

Example usage:

@
app :: App ()
app = do
    answer <- Iris.yesno "Would you like to proceed?" Iris.Yes
    case answer of
        Iris.Yes -> proceed
        Iris.No -> Iris.outLn "Aborting"

@

@since x.x.x.x
-}
yesno :: (MonadIO m, MonadReader (CliEnv cmd appEnv) m)
    => Text  
    -> YesNo 
    -> m YesNo 
yesno question defaultAnswer  = do
    interactiveMode <- asksCliEnv cliEnvInteractiveMode
    case  interactiveMode of
            NonInteractive -> pure defaultAnswer
            Interactive -> liftIO $ ask question >>= loop 
    where
        loop (Just a) = pure a
        loop Nothing =  ask "  Please answer (Y)es or (N)o " >>= loop

        ask q = do
            TIO.putStr q
            hFlush stdout
            parseYesNo <$> TIO.getLine 