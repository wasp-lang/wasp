module Wasp.Cli.Command.BuildStart
  ( buildStart,
  )
where

import Control.Monad.Except (throwError)
import System.Exit (ExitCode)
import Wasp.AppSpec (AppSpec)
import Wasp.Cli.Command (Command, commandError, getConsole, require)
import Wasp.Cli.Command.BuildStart.ArgumentsParser (buildStartArgsParser)
import Wasp.Cli.Command.BuildStart.Client (buildClient, startClient)
import Wasp.Cli.Command.BuildStart.Config (BuildStartConfig (..), makeBuildStartConfig)
import Wasp.Cli.Command.BuildStart.Server (buildServer, startServer)
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.Compile (analyze)
import Wasp.Cli.Command.Message (cliSendMessageC, runTaskC)
import Wasp.Cli.Command.Require.GeneratedApp (GeneratedAppIsProduction (GeneratedAppIsProduction))
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import Wasp.Cli.Command.Require.WaspSpecAvailable (WaspSpecAvailable (WaspSpecAvailable))
import Wasp.Cli.Console (Console (..))
import Wasp.Cli.ProjectRunConfig (showRunConfigUrls)
import Wasp.Cli.Util.Parser (withArguments)
import qualified Wasp.Job as Job
import qualified Wasp.Message as Msg
import Wasp.Util (checkExitSuccess)

buildStart :: Arguments -> Command ()
buildStart = withArguments buildStartArgsParser $ \args -> do
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

  buildAndStartServerAndClient appSpec config

buildAndStartServerAndClient :: AppSpec -> BuildStartConfig -> Command ()
buildAndStartServerAndClient appSpec config = do
  writeRawOutput <- (.writeRawOutput) <$> getConsole

  runTaskC "Building client..." "Client built." $
    Job.run writeRawOutput (Job.prefixWith Job.WebApp $ buildClient config)
      >>= throwOnExitFailure "Building client failed." "Building the client"

  runTaskC "Building server..." "Server built." $
    Job.run writeRawOutput (Job.prefixWith Job.Server $ buildServer config)
      >>= throwOnExitFailure "Building server failed." "Building the server"

  cliSendMessageC $ Msg.Start "Starting client and server..."
  cliSendMessageC
    $ Msg.Info
    $ showRunConfigUrls appSpec config.projectRunConfig

  clientOrServerExitCode <-
    Job.run writeRawOutput $
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
    throwOnExitFailure errorTitle failedStep =
      either (throwError . commandError errorTitle) return . checkExitSuccess failedStep
