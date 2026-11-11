module Wasp.Cli.Command.BuildStart
  ( buildStart,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import System.Exit (ExitCode (..))
import Wasp.AppSpec (AppSpec)
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
import Wasp.Cli.ProjectRunConfig (showRunConfigUrls)
import Wasp.Cli.Util.Parser (withArguments)
import qualified Wasp.Job as Job
import qualified Wasp.Message as Msg
import Wasp.Util (checkExitSuccess)
import Wasp.Util.TaskStatus (runTaskWithStatusReport)

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
  runBuildStep "Building the client" "Built the client" "Failed to build the client" $ buildClient config
  runBuildStep "Building the server" "Built the server" "Failed to build the server" $ buildServer config

  cliSendMessageC $ Msg.Start "Starting client and server..."
  cliSendMessageC
    $ Msg.Info
    $ showRunConfigUrls appSpec config.projectRunConfig

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

    runBuildStep :: String -> String -> String -> Job.Job ExitCode -> Command ()
    runBuildStep runningMessage successMessage failureMessage buildJob =
      liftIO (runTaskWithStatusReport runningMessage successMessage failureMessage runBuildJob)
        >>= either (throwError . CommandError failureMessage) return
      where
        runBuildJob = do
          (exitCode, buildOutput) <- Job.run $ Job.captureOutput buildJob
          return $ case exitCode of
            ExitSuccess -> Right ()
            ExitFailure code
              | T.null buildOutput -> Left $ "Exited with code " ++ show code
              | otherwise -> Left $ T.unpack buildOutput

    throwOnExitFailure :: String -> String -> ExitCode -> Command ()
    throwOnExitFailure errorTitle failedStep =
      either (throwError . CommandError errorTitle) return . checkExitSuccess failedStep
