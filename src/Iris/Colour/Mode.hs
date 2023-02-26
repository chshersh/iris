{- |
Module                  : Iris.Colour.Mode
Copyright               : 2022 Dmitrii Kovanikov
SPDX-License-Identifier : MPL-2.0
Maintainer              : Dmitrii Kovanikov <kovanikov@gmail.com>
Stability               : Experimental
Portability             : Portable

The 'ColourMode' data type that allows disabling and enabling of
colouring.

@since 0.0.0.0
-}

module Iris.Colour.Mode
    ( ColourMode (..)
    , detectColourMode

    -- * Internal
    , handleColourMode
    ) where

import Data.Char (toLower, toUpper)
import Data.Maybe (isJust)
import System.Console.ANSI (hSupportsANSIColor)
import System.Environment (lookupEnv)
import System.IO (Handle)

import Iris.Cli.Colour (ColourOption (..))

{- | Data type that tells whether the colouring is enabled or
disabled. Its value is detected automatically on application start and
stored in 'Iris.Env.CliEnv'.

@since 0.0.0.0
-}
data ColourMode
    -- | @since 0.0.0.0
    = DisableColour

    -- | @since 0.0.0.0
    | EnableColour
    deriving stock
        ( Show     -- ^ @since 0.0.0.0
        , Eq       -- ^ @since 0.0.0.0
        , Ord      -- ^ @since 0.0.0.0
        , Enum     -- ^ @since 0.0.0.0
        , Bounded  -- ^ @since 0.0.0.0
        )

{- | Returns 'ColourMode' of a 'Handle' ignoring environment and CLI options.
You can use this function on output 'Handle's to find out whether they support
colouring or not.

Use a function like this to check whether you can print with colour
to terminal:

@
'handleColourMode' 'System.IO.stdout'
@

@since 0.0.0.0
-}
handleColourMode :: Handle -> IO ColourMode
handleColourMode handle = do
    supportsANSI <- hSupportsANSIColor handle
    pure $ if supportsANSI then EnableColour else DisableColour

{- | This function performs a full check of the 'Handle' colouring support, env
variables and user-specified settings to detect whether the given handle
supports colouring.

Per CLI Guidelines, the algorithm for detecting the colouring support is the
following:

__Disable color if your program is not in a terminal or the user requested it.
These things should disable colors:__

* @stdout@ or @stderr@ is not an interactive terminal (a TTY). It’s best to
  individually check—if you’re piping stdout to another program, it’s still
  useful to get colors on stderr.
* The @NO_COLOR@ environment variable is set.
* The @TERM@ environment variable has the value @dumb@.
* The user passes the option @--no-color@.
* You may also want to add a @MYAPP_NO_COLOR@ environment variable in case users
  want to disable color specifically for your program.

ℹ️ Iris performs this check on the application start automatically so you don't
need to call this function manually.

@since x.x.x.x
-}
detectColourMode
    :: Handle
    -- ^ A terminal handle (e.g. 'System.IO.stderr')
    -> ColourOption
    -- ^ User settings
    -> Maybe String
    -- ^ Application name
    -> IO ColourMode
detectColourMode handle colour maybeAppName = case colour of
    Never -> pure DisableColour
    Always -> pure EnableColour
    Auto -> autoDetectColour
  where
    autoDetectColour :: IO ColourMode
    autoDetectColour = disabledToMode <$> checkIfDisabled

    disabledToMode :: Bool -> ColourMode
    disabledToMode isDisabled =
        if isDisabled then DisableColour else EnableColour

    checkIfDisabled :: IO Bool
    checkIfDisabled = orM
        [ isHandleColouringDisabled
        , hasNoColourEnvVars
        , isTermDumb
        ]

    isHandleColouringDisabled :: IO Bool
    isHandleColouringDisabled = (== DisableColour) <$> handleColourMode handle

    hasNoColourEnvVars :: IO Bool
    hasNoColourEnvVars = orM $ map hasEnvVar allVarNames

    isTermDumb :: IO Bool
    isTermDumb = lookupEnv "TERM" >>= \mVal -> pure $ case mVal of
        Nothing -> False
        Just val -> map toLower val == "dumb"

    hasEnvVar :: String -> IO Bool
    hasEnvVar var = isJust <$> lookupEnv var

    noColourVarNames :: [String]
    noColourVarNames = ["NO_COLOR", "NO_COLOUR"]

    prepend :: String -> String -> String
    prepend appName envName = map toUpper appName <> "_" <> envName

    allVarNames :: [String]
    allVarNames = case maybeAppName of
        Nothing      -> noColourVarNames
        Just appName -> noColourVarNames <> map (prepend appName) noColourVarNames

(||^) :: Monad m => m Bool -> m Bool -> m Bool
mx ||^ my = do
  x <- mx
  if x
    then pure True
    else my

orM :: Monad m => [m Bool] -> m Bool
orM = foldr (||^) (pure False)