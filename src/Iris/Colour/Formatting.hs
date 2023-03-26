{-# LANGUAGE FlexibleContexts #-}

{- |
Module                  : Iris.Colour.Formatting
Copyright               : (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier : MPL-2.0
Maintainer              : Dmitrii Kovanikov <kovanikov@gmail.com>
Stability               : Experimental
Portability             : Portable

Helper functions to print with colouring.

@since 0.0.0.0
-}
module Iris.Colour.Formatting (
    putStdoutColouredLn,
    putStderrColouredLn,
    putStdoutColoured,
    putStderrColoured,
) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import Data.Text (Text)

import Iris.Colour.Mode (ColourMode (..))
import Iris.Env (CliEnv (..), asksCliEnv)
import qualified Iris.IO as IO

{- | Print 'Text' to 'System.IO.stdout' by providing a custom
formatting function.

This works especially well with the @pretty-terminal@ package:

@
'putStdoutColouredLn'
    (style Bold . color Green)
    "my message"
@

@since 0.0.0.0
-}
putStdoutColouredLn
    :: ( MonadReader (CliEnv cmd appEnv) m
       , MonadIO m
       )
    => (Text -> Text)
    -> Text
    -> m ()
putStdoutColouredLn formatWithColour str = do
    colourMode <- asksCliEnv cliEnvStdoutColourMode
    liftIO $ IO.outLn $ case colourMode of
        DisableColour -> str
        EnableColour -> formatWithColour str

{- | Print 'Text' to 'System.IO.stderr' by providing a custom
formatting function.

This works especially well with the @pretty-terminal@ package:

@
'putStderrColouredLn'
    (style Bold . color Green)
    "my message"
@

@since 0.0.0.0
-}
putStderrColouredLn
    :: ( MonadReader (CliEnv cmd appEnv) m
       , MonadIO m
       )
    => (Text -> Text)
    -> Text
    -> m ()
putStderrColouredLn formatWithColour str = do
    colourMode <- asksCliEnv cliEnvStderrColourMode
    liftIO $ IO.errLn $ case colourMode of
        DisableColour -> str
        EnableColour -> formatWithColour str

{- | Print 'Text' to 'System.IO.stdout' by providing a custom
formatting function. Doesn't breaks output line that differs from
`putStdoutColouredLn`

This works especially well with the @pretty-terminal@ package:

@
'putStdoutColoured'
    (style Bold . color Green)
    "my message"
@

@since 0.1.0.0
-}
putStdoutColoured
    :: ( MonadReader (CliEnv cmd appEnv) m
       , MonadIO m
       )
    => (Text -> Text)
    -> Text
    -> m ()
putStdoutColoured formatWithColour str = do
    colourMode <- asksCliEnv cliEnvStdoutColourMode
    liftIO $ IO.out $ case colourMode of
        DisableColour -> str
        EnableColour -> formatWithColour str

{- | Print 'Text' to 'System.IO.stderr' by providing a custom
formatting function. Doesn't breaks output line that differs from
`putStderrColouredLn`

This works especially well with the @pretty-terminal@ package:

@
'putStderrColoured'
    (style Bold . color Green)
    "my message"
@

@since 0.1.0.0
-}
putStderrColoured
    :: ( MonadReader (CliEnv cmd appEnv) m
       , MonadIO m
       )
    => (Text -> Text)
    -> Text
    -> m ()
putStderrColoured formatWithColour str = do
    colourMode <- asksCliEnv cliEnvStderrColourMode
    liftIO $ IO.err $ case colourMode of
        DisableColour -> str
        EnableColour -> formatWithColour str
