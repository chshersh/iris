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
    , ToolCheckError (..)
    , ToolCheckException (..)
    , checkTool
    , need
    ) where

import Data.String (IsString (..))
import Data.Text (Text)
import System.Directory (findExecutable)
import System.Process (readProcess)
import Control.Exception (Exception, throwIO)
import Data.Foldable (for_)
import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Data.Text as Text


{- |

@since 0.0.0.0
-}
data Tool = Tool
    { -- | @since 0.0.0.0
      toolName     :: Text

      -- | @since 0.0.0.0
    , toolSelector :: Maybe ToolSelector
    }

{- |

@since 0.0.0.0
-}
instance IsString Tool where
    fromString :: String -> Tool
    fromString s = Tool
        { toolName     = fromString s
        , toolSelector = Nothing
        }

{- |

@since 0.0.0.0
-}
data ToolSelector = ToolSelector
    { -- | @since 0.0.0.0
      toolSelectorFunction   :: Text -> Bool

      -- | @since 0.0.0.0
    , toolSelectorVersionArg :: Maybe Text
    }

{- |

@since 0.0.0.0
-}
defaultToolSelector :: ToolSelector
defaultToolSelector = ToolSelector
    { toolSelectorFunction   = const True
    , toolSelectorVersionArg = Nothing
    }

{- |

@since 0.0.0.0
-}
data ToolCheckResult
    {- |

    @since x.x.x.x
    -}
    = ToolCheckError ToolCheckError
    {- |

    @since 0.0.0.0
    -}
    | ToolOk
    deriving stock
        ( Show  -- ^ @since 0.0.0.0
        , Eq    -- ^ @since 0.0.0.0
        )

{- |

@since x.x.x.x
-}
data ToolCheckError
    {- |

    @since x.x.x.x
    -}
    = ToolNotFound Text

    {- |

    @since x.x.x.x
    -}
    | ToolWrongVersion Text
    deriving stock
        ( Show  -- ^ @since x.x.x.x
        , Eq    -- ^ @since x.x.x.x
        )

{- |

@since 0.0.0.0
-}
checkTool :: Tool-> IO ToolCheckResult
checkTool Tool{..} = findExecutable (Text.unpack toolName) >>= \case
    Nothing  -> pure $ ToolCheckError $ ToolNotFound toolName
    Just exe -> case toolSelector of
        Nothing               -> pure ToolOk
        Just ToolSelector{..} -> case toolSelectorVersionArg of
            Nothing         -> pure ToolOk
            Just versionArg -> do
                toolVersionOutput <- readProcess exe [Text.unpack versionArg] ""
                let version = Text.strip $ Text.pack toolVersionOutput

                if toolSelectorFunction version
                then pure ToolOk
                else pure $ ToolCheckError $ ToolWrongVersion version

newtype ToolCheckException = ToolCheckException ToolCheckError
    deriving stock
        ( Show  -- ^ @since 0.0.0.0
        )

    deriving newtype
        ( Eq  -- ^ @since 0.0.0.0
        )

    deriving anyclass
        ( Exception  -- ^ @since 0.0.0.0
        )


{-|

__Throws:__ 'ToolCheckException' if can't find a tool or if it has wrong version.
@since 0.0.0.0
-}
need :: MonadIO m => [Tool] -> m ()
need tools =
    for_ tools $ \tool ->
        liftIO $ checkTool tool >>= \case
            ToolOk  -> pure ()
            (ToolCheckError toolErr) -> throwIO $ ToolCheckException toolErr
