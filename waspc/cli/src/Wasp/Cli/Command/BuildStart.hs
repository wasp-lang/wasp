module Wasp.Cli.Command.BuildStart
  ( buildStart,
  )
where

import Wasp.AppSpec.App.Deployment (DeploymentMode (..))
import Wasp.Cli.Command (Command, require)
import Wasp.Cli.Command.Build.Client (buildClient)
import Wasp.Cli.Command.BuildStart.ArgumentsParser (buildStartArgsParser)
import Wasp.Cli.Command.BuildStart.Client (startClient)
import Wasp.Cli.Command.BuildStart.Config (BuildStartConfig (..), makeBuildStartConfig)
import Wasp.Cli.Command.BuildStart.Server (buildServer, startServer)
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.Common (runAndPrintJob)
import Wasp.Cli.Command.Compile (analyze)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require.GeneratedApp (GeneratedAppIsProduction (GeneratedAppIsProduction))
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import Wasp.Cli.Command.Require.WaspSpecAvailable (WaspSpecAvailable (WaspSpecAvailable))
import Wasp.Cli.RunConfigs (showRunConfigUrls)
import Wasp.Cli.Util.Parser (withArguments)
import Wasp.Env (getEnvVars)
import qualified Wasp.Job.Except as ExceptJob
import qualified Wasp.Message as Msg
import Wasp.Project.BuildType (BuildType (Production))

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
  -- `wasp build` already built the client, but we build it again here so the `--client-env` vars apply.
  cliSendMessageC $ Msg.Start "Building client..."
  runAndPrintJob "Building client failed." $
    buildClient (getEnvVars config.clientRunConfig) config.projectDir
  cliSendMessageC $ Msg.Success "Client built."

  cliSendMessageC $ Msg.Start "Building server..."
  runAndPrintJob "Building server failed." $
    buildServer config
  cliSendMessageC $ Msg.Success "Server built."

  case config.deploymentMode of
    Single -> do
      cliSendMessageC $ Msg.Start "Starting the app..."
      cliSendMessageC $ Msg.Info $ showUrls config
      -- The server container serves the built client, so there is no
      -- separate client to start.
      runAndPrintJob "Starting Wasp app failed." $
        startServer config
    Split -> do
      cliSendMessageC $ Msg.Start "Starting client and server..."
      cliSendMessageC $ Msg.Info $ showUrls config
      runAndPrintJob "Starting Wasp app failed." $
        ExceptJob.race_
          (startClient config)
          (startServer config)
  where
    showUrls buildStartConfig =
      showRunConfigUrls
        Production
        buildStartConfig.deploymentMode
        (buildStartConfig.clientRunConfig, buildStartConfig.serverRunConfig)
