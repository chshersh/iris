{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module                  : Iris.IO
Copyright               : (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier : MPL-2.0
Maintainer              : Dmitrii Kovanikov <kovanikov@gmail.com>
Stability               : Experimental
Portability             : Portable

Useful IO Tools for other modules.

@since 0.0.0.0
-}

module Iris.IO
    ( ActionFlag (..)
    , MultipleChoiceOption(..)
    , MultipleChoicePrompt(..)
    , YesNo(..)
    , multipleChoiceQuestion
    , yesNoQuestion
    , writeFile
    ) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (intercalate, putStr, writeFile)
import qualified Data.ByteString.Char8 as C
import Data.Char (toLower)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isNothing)
import Data.Monoid (Alt (..))
import Iris.Colour.Formatting (putStdoutColouredLn)
import Iris.Env (CliEnv)
import Prelude hiding (writeFile)
import System.Directory (doesPathExist)
--import qualified Colourista

{-| Represents an option coupled with a way to parse it.
There is no need to provide a parsing function that accepts whitespaced only
inputs for the default option since 'multipleChoiceQuestion' will do so anyways.

@since 0.0.0.0
-}
data MultipleChoiceOption a = MultipleChoiceOption
    { displayName   :: ByteString  -- ^ @since 0.0.0.0
    , parsingFun    :: ByteString -> Maybe a  -- ^ @since 0.0.0.0
    }

{- | A prompt coupled with multiple choices and a way to parse them. 

For example:

@
MultipleChoicePrompt 
    { promptMessage="Are you sure?"
    , promptOptions=
        MultipleChoiceOption 
            { displayName="y"
            , parsingFun = \s -> if s == "y" then Just Yes else Nothing
            }
        :|
        [ MultipleChoiceOption 
            { displayName="n"
            , parsingFun = \s -> if s == "n" then Just No else Nothing
            }
        ]
    , defaultValue=Yes
    }
@

Will generate the following prompt:

@
Are you sure? [y]/n
@

The default option will always be the first one.

@since 0.0.0.0
-}
data MultipleChoicePrompt a = MultipleChoicePrompt
    { promptMessage :: ByteString  -- ^ @since 0.0.0.0
    , promptOptions :: NonEmpty (MultipleChoiceOption a) -- ^ @since 0.0.0.0
    , defaultValue  :: a  -- ^ @since 0.0.0.0
    }


{-| Prints a multiple choice question as per 'MultipleChoicePrompt'.

@since 0.0.0.0
-}
printMultipleChoiceQuestion 
    ::  ( MonadIO m
        , MonadReader (CliEnv cmd appEnv) m
        )
    => MultipleChoicePrompt a
    -> m ()
printMultipleChoiceQuestion (MultipleChoicePrompt question (defaultChoice :| rest) _) 
    = lPutStr (question <> " ") 
    >> putStdoutColouredLn 
        (\s -> BS.intercalate "/" $ makeDefaultColored s : fmap displayName rest)
        (displayName defaultChoice)
    where
        lPutStr = liftIO . BS.putStr
        makeDefaultColored = color . bracketed

        {- Enable whenever colourista updates :)
        color = Colourista.formatWith [Colourista.green, Colourista.bold]
        -}
        
        -- maybe consider ansi-terminal?
        boldGreen  = ("\ESC[1;38;5;82m" <>)
        reset      = (<> "\ESC[0m" )
        color  = reset . boldGreen

{-| A parser for the default value. Accepts any null or white space only string.
@since 0.0.0.0
-}
defaultParser :: a -> (ByteString -> Maybe a)
defaultParser returnValue s = if C.null . C.strip $ s 
    then pure returnValue
    else Nothing

{- | Prints out a multiple choice question, waits for the result, and returns
the first option from left to right that is parsed correctly. i.e:

@
>>> multipleChoiceQuestion
    MultipleChoicePrompt 
    { promptMessage="Are you sure?"
    , promptOptions=
        MultipleChoiceOption 
            { displayName="y"
            , parsingFun = \s -> if s == "y" then Just Yes else Nothing
            }
        :|
        [ MultipleChoiceOption 
            { displayName="n"
            , parsingFun = \s -> if s == "n" then Just No else Nothing
            }
        ]
    , defaultValue=Yes
    }
>>> y
Just Yes
@

@
>>> multipleChoiceQuestion
    MultipleChoicePrompt 
    { promptMessage="Are you sure?"
    , promptOptions=
        MultipleChoiceOption 
            { displayName="y"
            , parsingFun = \s -> if s == "y" then Just Yes else Nothing
            }
        :|
        [ MultipleChoiceOption 
            { displayName="n"
            , parsingFun = \s -> if s == "n" then Just No else Nothing
            }
        ]
    , defaultValue=Yes
    }
>>> 
Just Yes
@

@
>>> multipleChoiceQuestion
    MultipleChoicePrompt 
    { promptMessage="Are you sure?"
    , promptOptions=
        MultipleChoiceOption 
            { displayName="y"
            , parsingFun = \s -> if s == "y" then Just Yes else Nothing
            }
        :|
        [ MultipleChoiceOption 
            { displayName="n"
            , parsingFun = \s -> if s == "n" then Just No else Nothing
            }
        ]
    , defaultValue=Yes
    }
>>> n
Just No
@

@
>>> multipleChoiceQuestion
    MultipleChoicePrompt 
    { promptMessage="Are you sure?"
    , promptOptions=
        MultipleChoiceOption 
            { displayName="y"
            , parsingFun = \s -> if s == "y" then Just Yes else Nothing
            }
        :|
        [ MultipleChoiceOption 
            { displayName="n"
            , parsingFun = \s -> if s == "n" then Just No else Nothing
            }
        ]
    , defaultValue=Yes
    }
>> badInput
[badInput] Is not a valid option
Nothing
@

@since 0.0.0.0
-}
multipleChoiceQuestion
    ::  ( MonadIO m
        , MonadReader (CliEnv cmd appEnv) m
        )
    => MultipleChoicePrompt a
    -> m (Maybe a)
multipleChoiceQuestion mcp = do
    printMultipleChoiceQuestion mcp
    answer <- C.pack <$> liftIO getLine 
    let 
        chosen = getAlt 
            . (Alt (defaultParser (defaultValue mcp) answer) <>) 
            . foldMap (Alt  . ($ answer) . parsingFun) 
            . promptOptions 
            $ mcp
    when (isNothing chosen) $ 
        (liftIO . BS.putStr) $ (color . bracketed) answer <> " Is not a valid option.\n"
    pure chosen
    
    where 
        {- Enable whenever colourista updates :)
        color = Colourista.formatWith [Colourista.red, Colourista.bold]
        -}
        
        -- maybe consider ansi-terminal?
        boldRed  = ("\ESC[1;38;5;196m" <>)
        reset      = (<> "\ESC[0m" )
        color  = reset . boldRed


{-| Shortcut for Yes/No questions.

@since 0.0.0.0
-}
data YesNo = Yes | No

{- | A shortcut for yes/no questions.
@since 0.0.0.0
-}
yesNoQuestion 
    ::  ( MonadIO m
        , MonadReader (CliEnv cmd appEnv) m
        )
    => ByteString  -- ^ Prompting message.
    -> m (Maybe YesNo)
yesNoQuestion confirmationMessage= multipleChoiceQuestion $ 
    MultipleChoicePrompt 
        { promptMessage=confirmationMessage
        , promptOptions=
            MultipleChoiceOption {displayName="y", parsingFun=parseYes}
            :|
            [MultipleChoiceOption {displayName="n", parsingFun=parseNo}]
        , defaultValue=Yes
        }
    where 
        parseYes s= if (`elem` ["y","yes"]) . standarize $ s
            then Just Yes
            else Nothing

        parseNo s= if (`elem` ["n","no"]) . standarize $ s
            then Just No
            else Nothing 


{- | Determines whether we have to ask for confirmation on a given action
or perform it without asking.
@since 0.0.0.0
-}
data ActionFlag 
    = Ask 
    | Force 
    deriving stock Show


{- | Writes to a file, prompting for confirmation in case any object exists in the
given filepath.
@since 0.0.0.0
-}
writeFile
    ::  ( MonadIO m
        , MonadReader (CliEnv cmd appEnv) m
        )
    => ActionFlag 
    -> FilePath 
    -> ByteString 
    -> m ()
writeFile action filepath content = case action of 
    Force -> liftIO overwriteFile
    Ask   -> liftIO (doesPathExist filepath)
        >>= \pathExists -> if pathExists 
            then askForConfirmation 
            else liftIO overwriteFile
    where
        overwriteFile :: IO ()
        overwriteFile = BS.writeFile filepath content

        askForConfirmation 
            ::  ( MonadIO m
                , MonadReader (CliEnv cmd appEnv) m
                )
            => m ()
        askForConfirmation = getAnswer
            >>= \case
                Just Yes  -> liftIO overwriteFile
                _   -> pure ()

        confirmationMessage :: ByteString
        confirmationMessage = "File: " 
            <> (quoted . C.pack) filepath 
            <> " already exists, would you like to overwrite it?"
        
        getAnswer 
            ::  ( MonadIO m
                , MonadReader (CliEnv cmd appEnv) m
                )
            => m (Maybe YesNo)
        getAnswer = yesNoQuestion confirmationMessage

-------------------------
-- Formatting Functions
-------------------------

{-| Encloses a 'ByteString' in brackets. i.e:

@
>>>bracketed "I'm trapped"
"[I'm Trapped]"
@

@since 0.0.0.0
-}
bracketed :: ByteString -> ByteString
bracketed s = "[" <> s <> "]"

{-| Encloses a 'ByteString' in quotes. i.e:

@
>>> bracketed "I'm trapped"
"\"I'm Trapped\""
@

@since 0.0.0.0
-}
quoted :: ByteString -> ByteString
quoted string = "\"" <> string <> "\"" 


{-| Strips the spaces from a 'ByteString' and makes it all
lowercase. i.e:

@
>>> standarize "  Lorem Ipsum  "
"lorem ipsum"
@

@since 0.0.0.0
-}
standarize :: ByteString -> ByteString
standarize = C.strip . C.map toLower