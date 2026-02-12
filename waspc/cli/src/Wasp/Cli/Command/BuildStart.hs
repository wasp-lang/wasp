module Wasp.Cli.Command.BuildStart
  ( buildStart,
  )
where

import Control.Concurrent.Async (concurrently)
import Control.Concurrent.Chan (newChan)
import Control.Monad (when)
import Control.Monad.Except (MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Wasp.Cli.Command (Command, CommandError (CommandError), require)
import Wasp.Cli.Command.Build.DockerBuildContext (prepareFilesNecessaryForDockerBuild)
import Wasp.Cli.Command.BuildStart.ArgumentsParser (buildStartArgsParser)
import Wasp.Cli.Command.BuildStart.Client (buildClient, buildSsr, startClient)
import Wasp.Cli.Command.BuildStart.Config (BuildStartConfig, buildDir, makeBuildStartConfig)
import Wasp.Cli.Command.BuildStart.Server (buildServer, startServer)
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.Compile (analyze)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require (GeneratedCodeIsProduction (GeneratedCodeIsProduction), InWaspProject (InWaspProject))
import Wasp.Cli.Util.Parser (withArguments)
import Wasp.Job.Except (ExceptJob)
import qualified Wasp.Job.Except as ExceptJob
import Wasp.Job.IO (readJobMessagesAndPrintThemPrefixed)
import qualified Wasp.Message as Msg
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Page as Page

buildStart :: Arguments -> Command ()
buildStart = withArguments "wasp build start" buildStartArgsParser $ \args -> do
  GeneratedCodeIsProduction _ <- require

  InWaspProject waspProjectDir <- require
  appSpec <- analyze waspProjectDir

  -- TODO: Find a way to easily check we can connect to the DB. We'd like to
  -- throw a clear error if not available. (See #2858)
  --
  -- It is not a big problem right now, because Prisma will fail shortly after
  -- the server starts if the DB is not running anyway, and with a very clear
  -- error message that we print.

  config <- makeBuildStartConfig appSpec args waspProjectDir

  liftIO (prepareFilesNecessaryForDockerBuild waspProjectDir (buildDir config)) >>= \case
    Left err -> throwError $ CommandError "Failed to prepare files necessary for docker build" err
    Right () -> return ()

  let ssrEnabled = any ((== Just True) . Page.ssr . snd) (AS.getPages appSpec)
  buildAndStartServerAndClient config ssrEnabled

buildAndStartServerAndClient :: BuildStartConfig -> Bool -> Command ()
buildAndStartServerAndClient config ssrEnabled = do
  cliSendMessageC $ Msg.Start "Building client..."
  runAndPrintJob "Building client failed." $
    buildClient config
  cliSendMessageC $ Msg.Success "Client built."

  when ssrEnabled $ do
    cliSendMessageC $ Msg.Start "Building SSR bundle..."
    runAndPrintJob "Building SSR bundle failed." $
      buildSsr config
    cliSendMessageC $ Msg.Success "SSR bundle built."

  cliSendMessageC $ Msg.Start "Building server..."
  runAndPrintJob "Building server failed." $
    buildServer config
  cliSendMessageC $ Msg.Success "Server built."

  cliSendMessageC $ Msg.Start "Starting client and server..."
  runAndPrintJob "Starting Wasp app failed." $
    ExceptJob.race_
      (startClient config ssrEnabled)
      (startServer config)
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
