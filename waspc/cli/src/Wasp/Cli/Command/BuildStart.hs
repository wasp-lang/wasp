module Wasp.Cli.Command.BuildStart
  ( buildStart,
  )
where

import Control.Concurrent (Chan, newChan)
import qualified Control.Concurrent.Async as Async
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (liftIO)
import System.Exit (ExitCode (..))
import Wasp.Cli.Command (Command, CommandError (CommandError), require)
import Wasp.Cli.Command.BuildStart.ArgumentsParser (buildStartArgsParser)
import Wasp.Cli.Command.BuildStart.Client (buildClient, startClient)
import Wasp.Cli.Command.BuildStart.Config (BuildStartConfig, makeBuildStartConfig)
import Wasp.Cli.Command.BuildStart.Server (buildServer, startServer)
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.Compile (analyze)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require.GeneratedApp (GeneratedAppIsProduction (GeneratedAppIsProduction))
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import Wasp.Cli.Command.Require.WaspSpecAvailable (WaspSpecAvailable (WaspSpecAvailable))
import Wasp.Cli.Util.Parser (withArguments)
import qualified Wasp.Job as Job
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
  runAndPrintJobOutput (Job.runJob $ buildClient config)
    >>= throwOnExitFailure "Building client failed."
  cliSendMessageC $ Msg.Success "Client built."

  cliSendMessageC $ Msg.Start "Building server..."
  runAndPrintJobOutput (Job.runJob $ buildServer config)
    >>= throwOnExitFailure "Building server failed."
  cliSendMessageC $ Msg.Success "Server built."

  cliSendMessageC $ Msg.Start "Starting client and server..."
  firstExit <-
    runAndPrintJobOutput $ \events ->
      Async.race
        (Job.runJob (startClient config) events)
        (Job.runJob (startServer config) events)
  case firstExit of
    Left clientExit -> throwOnExitFailure "Serving client failed." clientExit
    Right serverExit -> throwOnExitFailure "Running server failed." serverExit
  where
    runAndPrintJobOutput :: (Chan Job.JobEvent -> IO a) -> Command a
    runAndPrintJobOutput run = liftIO $ do
      chan <- newChan
      fst
        <$> Async.concurrently
          (run chan)
          (Output.printEventsPrefixedUntilExit chan)

    throwOnExitFailure :: String -> ExitCode -> Command ()
    throwOnExitFailure _ ExitSuccess = return ()
    throwOnExitFailure errorTitle (ExitFailure code) =
      throwError $
        CommandError
          errorTitle
          ("Process exited with code " <> show code <> ".")
