{- |
Module                  : Iris.Browse
Copyright               : (c) 2020 Kowainik
                          (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier : MPL-2.0
Maintainer              : Dmitrii Kovanikov <kovanikov@gmail.com>
Stability               : Experimental
Portability             : Portable

Implements a function that opens a given file in a browser.

@since 0.0.0.0
-}

module Iris.Browse
    ( openInBrowser
    , BrowseException (..)
    ) where

import Control.Exception (Exception, throwIO)
import System.Directory (findExecutable)
import System.Environment (lookupEnv)
import System.Info (os)
import System.Process (callCommand, showCommandForUser)


{- | Exception thrown by 'openInBrowser'.

@since 0.0.0.0
-}
newtype BrowseException
    -- | Can't find a browser application. Stores the current OS inside.
    --
    -- @since 0.0.0.0
    = BrowserNotFoundException String
    deriving stock
        ( Show  -- ^ @since 0.0.0.0
        )

    deriving newtype
        ( Eq  -- ^ @since 0.0.0.0
        )

    deriving anyclass
        ( Exception  -- ^ @since 0.0.0.0
        )


{- | Open a given file in a browser. The function has the following algorithm:

* Check the @BROWSER@ environment variable
* If it's not set, try to guess browser depending on OS
* If unsuccsessful, print a message

__Throws:__ 'BrowseException' if can't find a browser.

@since 0.0.0.0
-}
openInBrowser :: FilePath -> IO ()
openInBrowser file = lookupEnv "BROWSER" >>= \case
    Just browser -> runCommand browser [file]
    Nothing -> case os of
        "darwin"  -> runCommand "open" [file]
        "mingw32" -> runCommand "cmd"  ["/c", "start", file]
        curOs     -> do
            browserExe <- findFirstExecutable
                [ "xdg-open"
                , "cygstart"
                , "x-www-browser"
                , "firefox"
                , "opera"
                , "mozilla"
                , "netscape"
                ]
            case browserExe of
                Just browser -> runCommand browser [file]
                Nothing      -> throwIO $ BrowserNotFoundException curOs

-- | Execute a command with arguments.
runCommand :: FilePath -> [String] -> IO ()
runCommand cmd args = do
    let cmdStr = showCommandForUser cmd args
    putStrLn $ "⚙  " ++ cmdStr
    callCommand cmdStr

findFirstExecutable :: [FilePath] -> IO (Maybe FilePath)
findFirstExecutable = \case
    [] -> pure Nothing
    exe:exes -> findExecutable exe >>= \case
        Nothing   -> findFirstExecutable exes
        Just path -> pure $ Just path

{-
------------------------
-- Original source code:
------------------------

{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Contains implementation of a function that opens a given file in a
browser.
-}

module Stan.Browse
    ( openBrowser
    ) where

import Colourista (errorMessage, infoMessage)
import System.Directory (findExecutable)
import System.Info (os)
import System.Process (callCommand, showCommandForUser)


{- | Open a given file in a browser. The function has the following algorithm:

* Check the @BROWSER@ environment variable
* If it's not set, try to guess browser depending on OS
* If unsuccsessful, print a message
-}
openBrowser :: FilePath -> IO ()
openBrowser file = lookupEnv "BROWSER" >>= \case
    Just browser -> runCommand browser [file]
    Nothing -> case os of
        "darwin"  -> runCommand "open" [file]
        "mingw32" -> runCommand "cmd"  ["/c", "start", file]
        curOs    -> do
            browserExe <- findFirstExecutable
                [ "xdg-open"
                , "cygstart"
                , "x-www-browser"
                , "firefox"
                , "opera"
                , "mozilla"
                , "netscape"
                ]
            case browserExe of
                Just browser -> runCommand browser [file]
                Nothing -> do
                    errorMessage $ "Cannot guess browser for the OS: " <> toText curOs
                    infoMessage "Please set the $BROWSER environment variable to a web launcher"
                    exitFailure

-- | Execute a command with arguments.
runCommand :: FilePath -> [String] -> IO ()
runCommand cmd args = do
    let cmdStr = showCommandForUser cmd args
    putStrLn $ "⚙  " ++ cmdStr
    callCommand cmdStr

findFirstExecutable :: [FilePath] -> IO (Maybe FilePath)
findFirstExecutable = \case
    [] -> pure Nothing
    exe:exes -> findExecutable exe >>= \case
        Nothing   -> findFirstExecutable exes
        Just path -> pure $ Just path
-}
