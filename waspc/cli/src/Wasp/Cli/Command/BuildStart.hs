module Wasp.Cli.Command.BuildStart
  ( buildStart,
  )
where

import Control.Concurrent.Async (concurrently)
import Control.Concurrent.Chan (newChan)
import Control.Monad.Except (MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Wasp.Cli.Command (Command, CommandError (CommandError), require)
import Wasp.Cli.Command.BuildStart.App (buildApp, startApp)
import Wasp.Cli.Command.BuildStart.ArgumentsParser (buildStartArgsParser)
import Wasp.Cli.Command.BuildStart.Config (BuildStartConfig, makeBuildStartConfig)
import qualified Wasp.Cli.Command.BuildStart.Config as Config
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.Compile (analyze)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require.GeneratedApp (GeneratedAppIsProduction (GeneratedAppIsProduction))
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import Wasp.Cli.Command.Require.WaspSpecAvailable (WaspSpecAvailable (WaspSpecAvailable))
import Wasp.Cli.Util.Parser (withArguments)
import Wasp.Job.Except (ExceptJob)
import Wasp.Job.IO (readJobMessagesAndPrintThemPrefixed)
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

  buildAndStartApp config

buildAndStartApp :: BuildStartConfig -> Command ()
buildAndStartApp config = do
  cliSendMessageC $ Msg.Start "Building the app's Docker image..."
  runAndPrintJob "Building the app failed." $
    buildApp config
  cliSendMessageC $ Msg.Success "App built."

  cliSendMessageC $ Msg.Start $ "Starting the app on " <> Config.appUrl config <> "..."
  runAndPrintJob "Starting Wasp app failed." $
    startApp config
  where
    runAndPrintJob :: String -> ExceptJob -> Command ()
    runAndPrintJob errorMessage job = do
      liftIO (runAndPrintJobIO job)
        >>= either (throwError . CommandError errorMessage) return

    runAndPrintJobIO :: ExceptJob -> IO (Either String ())
    runAndPrintJobIO job = do
      chan <- newChan
      (result, _) <-
        concurrently
          (runExceptT $ job chan)
          (readJobMessagesAndPrintThemPrefixed chan)
      return result
