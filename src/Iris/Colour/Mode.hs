{- |
Module                  : Iris.Colour.Mode
Copyright               : (c) 2020 Kowainik
                          (c) 2022 Dmitrii Kovanikov
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
    , handleColourMode
    , actualHandleColourMode
    ) where

import Iris.Cli.Colour
import Iris.Colour.Detect (detectColourDisabled)
import System.Console.ANSI (hSupportsANSIColor)
import System.IO (Handle)


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

{- | Returns 'ColourMode' of a 'Handle' accounting for environment and CLI
options. You can use this function on output 'Handle's to find out whether
they support colouring or not.

Use a function like this to check whether you can print with colour
to terminal:

@
'handleColourMode' '(Just myappname)' 'AutoColour' 'System.IO.stdout'
@

@since x.x.x.x
-}
actualHandleColourMode :: Maybe String -> ColourOption -> Handle ->  IO ColourMode
actualHandleColourMode app colourOption handle = do
    handleMode <- handleColourMode handle
    colourDisabled <- detectColourDisabled app
    pure $ case (handleMode,colourOption,colourDisabled) of
        (DisableColour,_,_) -> DisableColour
        (_,AlwaysColour,_)  -> EnableColour
        (_,NeverColour,_)   -> DisableColour
        (_,_,True)          -> DisableColour
        (_,_,False)         -> EnableColour

{-
------------------------
-- Original source code:
------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

The 'ColourMode' data type that allows disabling and enabling of
colouring. Implemented using the [Implicit Parameters](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/glasgow_exts.html#implicit-parameters)
GHC feature.

By default, all formatting and printing functions in @colourista@
print with colour. However, you control this behaviour by adding the
@HasColourMode@ constraint to your functions and setting the value of
the implicit @?colourMode@ variable.

@since 0.2.0.0
-}

module Colourista.Mode
    ( ColourMode (..)
    , HasColourMode
    , withColourMode
    , handleColourMode
    ) where

import System.IO (Handle)
import System.Console.ANSI (hSupportsANSIWithoutEmulation)
import Data.String (IsString)

import GHC.Classes (IP (..))


{- | Data type that tells whether the colouring is enabled or
disabled. It's used with the @-XImplicitParams@ GHC extension.

@since 0.2.0.0
-}
data ColourMode
    = DisableColour
    | EnableColour
    deriving stock (Show, Eq, Enum, Bounded)

{- | Magic instance to set the value of the implicit variable
@?colourMode@ to 'EnableColour' by default. Equivalent to the
following code:

@
?colourMode = 'EnableColour'
@

However, you still can override @?colourMode@ with any possible value.

@since 0.2.0.0
-}
instance IP "colourMode" ColourMode where
    ip = EnableColour

{- | Constraint that stores 'ColourMode' as an implicit parameter.

@since 0.2.0.0
-}
type HasColourMode = (?colourMode :: ColourMode)

{- | Helper function for writing custom formatter. The function takes
'ColourMode' from the implicit parameter context and either returns a
given string or an empty string.

@since 0.2.0.0
-}
withColourMode :: (HasColourMode, IsString str) => str -> str
withColourMode str = case ?colourMode of
    EnableColour  -> str
    DisableColour -> ""
{-# INLINE withColourMode #-}

{- | Returns 'ColourMode' of a 'Handle'. You can use this function on
output 'Handle's to find out whether they support colouring or
now. Use this function like this to check whether you can print with
colour to terminal:

@
'handleColourMode' 'System.IO.stdout'
@

Typical usage can look like this:

@
main :: IO ()
main = do
    colourMode <- 'handleColourMode' 'System.IO.stdout'
    let ?colourMode = fromMaybe 'DisableColour'
    'Colourista.IO.successMessage' "Success!"
@

@since 0.2.0.0
-}
handleColourMode :: Handle -> IO (Maybe ColourMode)
handleColourMode handle = do
    supportsANSI <- hSupportsANSIWithoutEmulation handle
    pure $ fmap
        (\supportsColour -> if supportsColour then EnableColour else DisableColour)
        supportsANSI
-}
