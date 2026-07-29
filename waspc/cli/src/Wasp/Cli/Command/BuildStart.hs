module Wasp.Cli.Command.BuildStart
  ( buildStart,
  )
where

import Control.Concurrent.Async (concurrently)
import Control.Concurrent.Chan (newChan)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (liftIO)
import Wasp.Cli.Command (Command, CommandError (CommandError), require)
import Wasp.Cli.Command.BuildStart.ArgumentsParser (buildStartArgsParser)
import Wasp.Cli.Command.BuildStart.Client (buildClient, startClient)
import Wasp.Cli.Command.BuildStart.Config (BuildStartConfig, makeBuildStartConfig)
import qualified Wasp.Cli.Command.BuildStart.Job as BuildStartJob
import Wasp.Cli.Command.BuildStart.Server (buildServer, startServer)
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.Compile (analyze)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require.GeneratedApp (GeneratedAppIsProduction (GeneratedAppIsProduction))
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import Wasp.Cli.Command.Require.WaspSpecAvailable (WaspSpecAvailable (WaspSpecAvailable))
import Wasp.Cli.Util.Parser (withArguments)
import qualified Wasp.Job.Output as Output
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
  runAndPrintJob "Building client failed." $
    buildClient config
  cliSendMessageC $ Msg.Success "Client built."

  cliSendMessageC $ Msg.Start "Building server..."
  runAndPrintJob "Building server failed." $
    buildServer config
  cliSendMessageC $ Msg.Success "Server built."

  cliSendMessageC $ Msg.Start "Starting client and server..."
  runAndPrintJob "Starting Wasp app failed." $
    BuildStartJob.race
      (startClient config)
      (startServer config)
  where
    runAndPrintJob :: String -> BuildStartJob.JobExecution -> Command ()
    runAndPrintJob errorMessage executeJob = do
      liftIO (runAndPrintJobIO executeJob)
        >>= either (throwError . CommandError errorMessage) return

    runAndPrintJobIO :: BuildStartJob.JobExecution -> IO (Either String ())
    runAndPrintJobIO executeJob = do
      chan <- newChan
      (result, _) <-
        concurrently
          (executeJob chan)
          (Output.printEventsPrefixedUntilExit chan)
      return result
