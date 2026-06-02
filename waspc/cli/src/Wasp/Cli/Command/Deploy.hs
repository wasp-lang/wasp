module Wasp.Cli.Command.Deploy
  ( deploy,
    parserInfo,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Options.Applicative as Opt
import System.Environment (getExecutablePath)
import Wasp.Cli.Command (Command, CommandError (CommandError), runCommand)
import qualified Wasp.Cli.Command.Call as Call
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), WaspSpecAvailable (WaspSpecAvailable), require)
import Wasp.Cli.Command.Telemetry (runWithTelemetry)
import qualified Wasp.Project.Deployment

parserInfo :: Opt.ParserInfo (IO ())
parserInfo =
  Opt.info
    (run <$> deployArgsParser)
    (Opt.progDesc "Deploy your Wasp app to cloud hosting providers." <> Opt.forwardOptions)
  where
    -- Deploy reports its own (arg-aware) telemetry, so it knows its command call here.
    run args = runWithTelemetry (Call.Deploy args) (runCommand (deploy args))

    deployArgsParser :: Opt.Parser [String]
    deployArgsParser =
      Opt.many $
        Opt.strArgument $
          Opt.metavar "DEPLOY_ARGS..."
            <> Opt.help "Arguments passed through to the deploy script (e.g. `fly setup my-app mia`)"

deploy :: [String] -> Command ()
deploy cmdArgs = do
  InWaspProject waspProjectDir <- require
  WaspSpecAvailable <- require
  deployResult <- liftIO $ do
    -- `getExecutablePath` has some caveats:
    --   https://frasertweedale.github.io/blog-fp/posts/2022-05-10-improved-executable-path-queries.html
    -- Once we upgrade to GHC 9.4 we should change to `executablePath`, but this should be ok for
    -- our purposes.
    waspExePath <- getExecutablePath
    Wasp.Project.Deployment.deploy waspExePath waspProjectDir cmdArgs
  either (throwError . CommandError "Deploy command failed") return deployResult
