{- |
Module                  : Iris.Browse
Copyright               : (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier : MPL-2.0
Maintainer              : Dmitrii Kovanikov <kovanikov@gmail.com>
Stability               : Experimental
Portability             : Portable

Implements a function that opens a given file in a browser.

@since 0.0.0.0
-}
module Iris.Browse (
    openInBrowser,
    BrowseException (..),
) where

import           Control.Exception  (Exception, throwIO)
import           System.Directory   (findExecutable)
import           System.Environment (lookupEnv)
import           System.Info        (os)
import           System.IO          (hPutStrLn, stderr)
import           System.Process     (callCommand, callProcess, readProcess,
                                     showCommandForUser)

{- | Exception thrown by 'openInBrowser'.

@since 0.0.0.0
-}
newtype BrowseException
    = -- | Can't find a browser application. Stores the current OS inside.
      --
      -- @since 0.0.0.0
      BrowserNotFoundException String
    deriving stock
        ( Show
          -- ^ @since 0.0.0.0
        )
    deriving newtype
        ( Eq
          -- ^ @since 0.0.0.0
        )
    deriving anyclass
        ( Exception
          -- ^ @since 0.0.0.0
        )

{- | Open a given file in a browser. The function has the following algorithm:

* Check the @BROWSER@ environment variable
* If it's not set, try to guess browser depending on OS
* If unsuccsessful, print a message

__Throws:__ 'BrowseException' if can't find a browser.

@since 0.0.0.0
-}
openInBrowser :: FilePath -> IO ()
openInBrowser file =
    lookupEnv "BROWSER" >>= \case
        Just browser -> runCommand browser [file]
        Nothing -> case os of
            "darwin" -> runCommand "open" [file]
            "mingw32" -> runCommand "cmd" ["/c", "start", file]
            curOs -> do
                browserExe <-
                    findFirstExecutable
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
    exe : exes ->
        findExecutable exe >>= \case
            Nothing -> findFirstExecutable exes
            Just path -> pure $ Just path

shell :: FilePath -> [String] -> IO ()
shell cmd args = do
     hPutStrLn stderr $ "⚙  " ++ cmd ++ " " ++ unwords args
     callProcess cmd args

-- | Run the command but don't print it
shellSilent :: FilePath -> [String] -> IO ()
shellSilent cmd args = callProcess cmd args

-- | Run the command, don't print it and return its stdout
shellRetSilent :: FilePath -> [String] -> IO String
shellRetSilent cmd args = readProcess cmd args ""

-- | Run the command, print it with prompt to stderr and return its stdout
shellRet :: FilePath -> [String] -> IO String
shellRet cmd args = do
      hPutStrLn stderr $ "⚙  " ++ cmd ++ " " ++ unwords args
      shellRetSilent cmd args
