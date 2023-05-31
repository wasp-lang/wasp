module Wasp.Cli.Command.Deploy.Parser (deployParser) where

import Options.Applicative
  ( Parser,
  )
import qualified Options.Applicative as O
import Wasp.Cli.Command.Call (CommandCall (Deploy))

deployParser :: Parser CommandCall
deployParser = Deploy <$> O.many deployRestArgs
  where
    deployRestArgs =
      O.strArgument $
        O.metavar "DEPLOY_ARGUMENTS"
          <> O.help "Currently only supports fly.io. See https://wasp-lang.dev/docs/deploying."
