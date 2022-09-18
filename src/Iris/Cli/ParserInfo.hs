{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ApplicativeDo #-}

module Iris.Cli.ParserInfo () where

import Iris.Settings (CliEnvSettings (..))

import qualified Options.Applicative as Opt
import qualified Iris.Env as Env
import Iris.Cli.Version (mkVersionParser)
import Iris.Cli.Interactive (interactiveModeP)
cmdParserInfo :: forall cmd appEnv . CliEnvSettings cmd appEnv -> Opt.ParserInfo (Env.Cmd cmd)
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
    cmdP :: Opt.Parser (Env.Cmd cmd)
    cmdP = do
      cmdInteractiveMode <- interactiveModeP
      cmdCmd <- cliEnvSettingsCmdParser

      pure Env.Cmd{..}