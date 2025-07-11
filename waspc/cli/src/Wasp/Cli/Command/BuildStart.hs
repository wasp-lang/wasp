module Wasp.Cli.Command.BuildStart
  ( buildStart,
  )
where

import Control.Concurrent.Async (concurrently)
import Control.Concurrent.Chan (newChan)
import Control.Monad.Except (MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Wasp.Cli.Command (Command, CommandError (CommandError), require)
import Wasp.Cli.Command.BuildStart.ArgumentsParser (parseBuildStartArgs)
import Wasp.Cli.Command.BuildStart.Client (buildClient, startClient)
import Wasp.Cli.Command.BuildStart.Config (BuildStartConfig, makeBuildStartConfig)
import Wasp.Cli.Command.BuildStart.Server (buildServer, startServer)
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.Compile (analyze)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require (BuildDirExists (BuildDirExists), InWaspProject (InWaspProject))
import Wasp.Job.Except (ExceptJob)
import qualified Wasp.Job.Except as ExceptJob
import Wasp.Job.IO (readJobMessagesAndPrintThemPrefixed)
import qualified Wasp.Message as Msg

buildStart :: Arguments -> Command ()
buildStart args = do
  buildStartArgs <-
    parseBuildStartArgs args
      & either (throwError . CommandError "Parsing build start arguments failed") return

  BuildDirExists _ <- require

  InWaspProject waspProjectDir <- require
  appSpec <- analyze waspProjectDir

  -- TODO: Find a way to easily check we can connect to the DB. We'd like to
  -- throw a clear error if not available. (See #2858)
  --
  -- It is not a big problem right now, because Prisma will fail shortly after
  -- the server starts if the DB is not running anyway, and with a very clear
  -- error message that we print.

  let config = makeBuildStartConfig appSpec waspProjectDir buildStartArgs

  buildAndStartServerAndClient config
  cliSendMessageC $ Msg.Success "Build and start completed successfully."

buildAndStartServerAndClient :: BuildStartConfig -> Command ()
buildAndStartServerAndClient config = do
  cliSendMessageC $ Msg.Start "Building client..."
  runAndPrintJob $ buildClient config
  cliSendMessageC $ Msg.Success "Client built."

  cliSendMessageC $ Msg.Start "Building server..."
  runAndPrintJob $ buildServer config
  cliSendMessageC $ Msg.Success "Server built."

  cliSendMessageC $ Msg.Start "Starting client and server..."
  runAndPrintJob $
    ExceptJob.race_
      (startClient config)
      (startServer config)
  where
    runAndPrintJob :: ExceptJob -> Command ()
    runAndPrintJob exceptJob = do
      liftIO (runAndPrintJobIO exceptJob)
        >>= either (throwError . CommandError "Starting built Wasp failed") return

    runAndPrintJobIO :: ExceptJob -> IO (Either String ())
    runAndPrintJobIO exceptJob = do
      chan <- newChan
      (result, _) <-
        concurrently
          (runExceptT $ exceptJob chan)
          (readJobMessagesAndPrintThemPrefixed chan)
      return result
