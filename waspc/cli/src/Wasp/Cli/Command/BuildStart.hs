module Wasp.Cli.Command.BuildStart
  ( buildStart,
  )
where

import Control.Monad.Except (throwError)
import System.Exit (ExitCode (..))
import Wasp.Cli.Command (Command, CommandError (CommandError), require)
import Wasp.Cli.Command.BuildStart.ArgumentsParser (buildStartArgsParser)
import Wasp.Cli.Command.BuildStart.Client (buildClient, startClient)
import Wasp.Cli.Command.BuildStart.Config (BuildStartConfig (..), makeBuildStartConfig)
import Wasp.Cli.Command.BuildStart.Server (buildServer, startServer)
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.Compile (analyze)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require.GeneratedApp (GeneratedAppIsProduction (GeneratedAppIsProduction))
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import Wasp.Cli.Command.Require.WaspSpecAvailable (WaspSpecAvailable (WaspSpecAvailable))
import Wasp.Cli.RunConfigs (showRunConfigUrls)
import Wasp.Cli.Util.Parser (withArguments)
import qualified Wasp.Job as Job
import qualified Wasp.Message as Msg

buildStart :: Arguments -> Command ()
buildStart = withArguments "wasp build start" buildStartArgsParser $ \args -> do
  ValidNodeAndNpm <- require
  GeneratedAppIsProduction _ <- require

  InWaspProject waspProjectDir <- require
  WaspSpecAvailable <- require
  appSpec <- analyze waspProjectDir

  -- TODO: Find a way to easily check we can connect to the DB. We'd like to
  -- throw a clear error if not available. (See #2858)
  --
  -- It is not a big problem right now, because Prisma will fail shortly after
  -- the server starts if the DB is not running anyway, and with a very clear
  -- error message that we print.

  config <- makeBuildStartConfig appSpec args waspProjectDir

  buildAndStartServerAndClient config

buildAndStartServerAndClient :: BuildStartConfig -> Command ()
buildAndStartServerAndClient config = do
  cliSendMessageC $ Msg.Start "Building client..."
  Job.run (Job.prefixWith Job.WebApp $ buildClient config)
    >>= throwOnExitFailure "Building client failed." "Building the client"
  cliSendMessageC $ Msg.Success "Client built."

  cliSendMessageC $ Msg.Start "Building server..."
  Job.run (Job.prefixWith Job.Server $ buildServer config)
    >>= throwOnExitFailure "Building server failed." "Building the server"
  cliSendMessageC $ Msg.Success "Server built."

  cliSendMessageC $ Msg.Start "Starting client and server..."
  cliSendMessageC
    $ Msg.Info
    $ showRunConfigUrls (config.clientRunConfig, config.serverRunConfig)

  clientOrServerExitCode <-
    Job.run $
      Job.race
        (Job.prefixWith Job.WebApp $ startClient config)
        (Job.prefixWith Job.Server $ startServer config)
  either
    (throwOnExitFailure startErrorTitle "Serving the client")
    (throwOnExitFailure startErrorTitle "Running the server")
    clientOrServerExitCode
  where
    startErrorTitle = "Starting Wasp app failed."

    throwOnExitFailure :: String -> String -> ExitCode -> Command ()
    throwOnExitFailure _ _ ExitSuccess = return ()
    throwOnExitFailure errorTitle failedStep (ExitFailure code) =
      throwError $
        CommandError
          errorTitle
          (failedStep <> " failed with exit code: " <> show code)
