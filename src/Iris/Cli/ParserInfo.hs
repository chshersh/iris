{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ApplicativeDo #-}

{- |
Module                  : Iris.Cli.ParserInfo
Copyright               : (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier : MPL-2.0
Maintainer              : Dmitrii Kovanikov <kovanikov@gmail.com>
Stability               : Experimental
Portability             : Portable

Parser information for the default CLI parser.

@since 0.0.0.0
-}

module Iris.Cli.ParserInfo (cmdParserInfo) where

import Iris.Cli.Internal ( Cmd (..) )
import Iris.Cli.Interactive (interactiveModeP)
import Iris.Settings (CliEnvSettings (..))
import Iris.Cli.Version (mkVersionParser)

import qualified Options.Applicative as Opt

{- |

@since 0.0.0.0
-}

cmdParserInfo :: forall cmd appEnv . CliEnvSettings cmd appEnv -> Opt.ParserInfo (Cmd cmd)
cmdParserInfo CliEnvSettings{..} = Opt.info
  ( Opt.helper
  <*> mkVersionParser cliEnvSettingsVersionSettings
  <*> cmdP
  )
  $ mconcat
      [ Opt.fullDesc
      , Opt.header cliEnvSettingsHeaderDesc
      , Opt.progDesc cliEnvSettingsProgDesc
      ]
  where
    cmdP :: Opt.Parser (Cmd cmd)
    cmdP = do
      cmdInteractiveMode <- interactiveModeP
      cmdCmd <- cliEnvSettingsCmdParser

      pure Cmd{..}
