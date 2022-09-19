{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ApplicativeDo #-}

module Iris.Cli.ParserInfo (cmdParserInfo) where

import Iris.Settings (CliEnvSettings (..), Cmd (..))

import qualified Options.Applicative as Opt
import Iris.Cli.Version (mkVersionParser)
import Iris.Cli.Interactive (interactiveModeP)
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