module Wasp.Cli.Command.Deploy.Parser (deploy) where

import Options.Applicative
  ( Alternative (many),
    CommandFields,
    Mod,
    Parser,
    help,
    metavar,
    strArgument,
  )
import Wasp.Cli.Command.Call (Call (Deploy))
import Wasp.Cli.Parser.Util (CommandType (CTNoIntersperse), mkWrapperCommand)

-- The rest of arguments will be supplied externally by chosen deploy tool.
deploy :: Mod CommandFields Call
deploy =
  mkWrapperCommand
    "deploy"
    CTNoIntersperse
    parseDeploy
    "Deploys your Wasp app to cloud hosting providers."

parseDeploy :: Parser Call
parseDeploy = Deploy <$> many deployRestArgs
  where
    deployRestArgs =
      strArgument
        ( metavar "DEPLOY_ARGUMENTS"
            <> help "Currently only support fly.io. See https://wasp-lang.dev/docs/deploying."
        )
