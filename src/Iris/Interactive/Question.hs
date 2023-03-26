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
    parseYesNo,
) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.IO (hFlush, stdout)

import Iris.Cli.Interactive (InteractiveMode (..))
import Iris.Env (CliEnv (..), asksCliEnv)
import qualified Iris.IO as IO

{-
@since x.x.x.x
-}
parseYesNo :: Text -> Maybe YesNo
parseYesNo t = case Text.toUpper . Text.strip $ t of
    "Y" -> Just Yes
    "YES" -> Just Yes
    "YS" -> Just Yes
    "N" -> Just No
    "NO" -> Just No
    _ -> Nothing

{- | Parsed as Yes: "Y", "YES", "YS" (lower- or uppercase)

Parsed as No: "N", "NO" (lower- or uppercase)

@since x.x.x.x
-}
data YesNo
    = No
    | Yes
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


\$ ./irisapp
Would you like to proceed? (yes/no)
I don't understand your answer: ''
Please, answer yes or no (or y, or n)
Would you like to proceed? (yes/no) ne
I don't understand your answer: 'ne'
Please, answer yes or no (or y, or n)
Would you like to proceed? (yes/no) NO
Aborting

@

@since x.x.x.x
-}
yesno
    :: (MonadIO m, MonadReader (CliEnv cmd appEnv) m)
    => Text
    -- ^ Question Text
    -> YesNo
    -- ^ Default answer when @--no-input@ is provided
    -> m YesNo
yesno question defaultAnswer = do
    interactiveMode <- asksCliEnv cliEnvInteractiveMode
    case interactiveMode of
        NonInteractive -> pure defaultAnswer
        Interactive -> liftIO loop
  where
    loop :: IO YesNo
    loop = do
        IO.out $ question <> " (yes/no) "
        hFlush stdout
        input <- Text.getLine
        case parseYesNo input of
            Just answer -> pure answer
            Nothing -> do
                IO.out $ "I don't understand your answer: '" <> input <> "'"
                IO.out "Please, answer yes or no (or y, or n)"
                loop
