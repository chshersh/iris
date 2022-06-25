{- |
Module                  : Iris.Tool
Copyright               : (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier : MPL-2.0
Maintainer              : Dmitrii Kovanikov <kovanikov@gmail.com>
Stability               : Experimental
Portability             : Portable

Utilities to check required tools and their minimal version for a CLI app.

@since 0.0.0.0
-}

module Iris.Tool
    ( -- * Types describing executable requirements
      Tool (..)
    , ToolSelector (..)
    , defaultToolSelector

      -- * Tool requirements check
    , ToolCheckResult (..)
    , checkTool
    ) where

import Data.String (IsString (..))
import Data.Text (Text)
import System.Directory (findExecutable)
import System.Process (readProcess)

import qualified Data.Text as Text


{- |

@since 0.0.0.0
-}
data Tool cmd = Tool
    { toolName     :: Text
    , toolSelector :: Maybe (ToolSelector cmd)
    }

{- |

@since 0.0.0.0
-}
instance IsString (Tool cmd) where
    fromString :: String -> Tool cmd
    fromString s = Tool
        { toolName     = fromString s
        , toolSelector = Nothing
        }

{- |

@since 0.0.0.0
-}
data ToolSelector cmd = ToolSelector
    { toolSelectorFunction   :: cmd -> Text -> Bool
    , toolSelectorVersionArg :: Maybe Text
    }

{- |

@since 0.0.0.0
-}
defaultToolSelector :: ToolSelector cmd
defaultToolSelector = ToolSelector
    { toolSelectorFunction   = \_cmd _version -> True
    , toolSelectorVersionArg = Nothing
    }

{- |

@since 0.0.0.0
-}
data ToolCheckResult
    {- |

    @since 0.0.0.0
    -}
    = ToolNotFound Text

    {- |

    @since 0.0.0.0
    -}
    | ToolWrongVersion Text

    {- |

    @since 0.0.0.0
    -}
    | ToolOk
    deriving stock (Show, Eq)

{- |

@since 0.0.0.0
-}
checkTool :: cmd -> Tool cmd -> IO ToolCheckResult
checkTool cmd Tool{..} = findExecutable (Text.unpack toolName) >>= \case
    Nothing  -> pure $ ToolNotFound toolName
    Just exe -> case toolSelector of
        Nothing               -> pure ToolOk
        Just ToolSelector{..} -> case toolSelectorVersionArg of
            Nothing         -> pure ToolOk
            Just versionArg -> do
                toolVersionOutput <- readProcess exe [Text.unpack versionArg] ""
                let version = Text.strip $ Text.pack toolVersionOutput

                if toolSelectorFunction cmd version
                then pure ToolOk
                else pure $ ToolWrongVersion version
