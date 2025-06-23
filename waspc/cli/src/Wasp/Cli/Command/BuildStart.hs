module Wasp.Cli.Command.BuildStart
  ( buildStart,
  )
where

import Control.Concurrent.Async (concurrently)
import Control.Concurrent.Chan (newChan)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Wasp.Cli.Command (Command, require)
import Wasp.Cli.Command.BuildStart.Client (buildClient, startClient)
import Wasp.Cli.Command.BuildStart.Config (BuildStartConfig (BuildStartConfig))
import Wasp.Cli.Command.BuildStart.Server (buildServer, startServer)
import Wasp.Cli.Command.Compile (analyze)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require (BuildDirExists (BuildDirExists), InWaspProject (InWaspProject))
import Wasp.Cli.Message (cliSendMessage)
import Wasp.Job.Except (ExceptJob)
import qualified Wasp.Job.Except as ExceptJob
import Wasp.Job.IO (readJobMessagesAndPrintThemPrefixed)
import qualified Wasp.Message as Msg

buildStart :: Command ()
buildStart = do
  BuildDirExists buildDir <- require

  InWaspProject waspProjectDir <- require
  appSpec <- analyze waspProjectDir

  -- TODO(carlos): Find a way to easily check we can connect to the DB. We'd
  -- like to throw a clear error if not available.
  --
  -- This is similar to what we do in the `DbConnectionEstablishedFromOutDir`
  -- requirement, but `DbConnectionEstablishedFromBuildDir`. Check the comment
  -- in the source code of `DbConnectionEstablishedFromOutDir` for an
  -- explanation of why we don't do it right now.
  --
  -- It is not a big problem right now, because Prisma will fail shortly after
  -- the server starts if the DB is not running anyway, and with a very clear
  -- error message that we print.

  let config = BuildStartConfig appSpec waspProjectDir buildDir

  result <-
    liftIO $
      runExceptT $
        buildAndStartServerAndClient config

  case result of
    Left err -> cliSendMessageC $ Msg.Failure "Build and start failed" err
    Right () -> cliSendMessageC $ Msg.Success "Build and start completed successfully."

buildAndStartServerAndClient :: BuildStartConfig -> ExceptT String IO ()
buildAndStartServerAndClient config = do
  liftIO $ cliSendMessage $ Msg.Start "Building client..."
  runAndPrintJob $ buildClient config
  liftIO $ cliSendMessage $ Msg.Success "Client built."

  liftIO $ cliSendMessage $ Msg.Start "Building server..."
  runAndPrintJob $ buildServer config
  liftIO $ cliSendMessage $ Msg.Success "Server built."

  liftIO $ cliSendMessage $ Msg.Start "Starting client and server..."
  runAndPrintJob $
    ExceptJob.race_
      (startClient config)
      (startServer config)
  where
    runAndPrintJob :: ExceptJob -> ExceptT String IO ()
    runAndPrintJob exceptJob = ExceptT $ do
      chan <- newChan
      (result, _) <-
        concurrently
          (runExceptT $ exceptJob chan)
          (readJobMessagesAndPrintThemPrefixed chan)
      return result
