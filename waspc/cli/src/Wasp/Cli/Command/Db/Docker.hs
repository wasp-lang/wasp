module Wasp.Cli.Command.Db.Docker
  ( ensureDockerDaemonAvailable,
    ensureImageAvailable,
    runDockerCommand,
    stopDatabase,
    removeContainer,
  )
where

import Control.Concurrent (Chan, newChan, readChan)
import Control.Concurrent.Async (concurrently)
import Control.Monad (forM_, when)
import qualified Control.Monad.Except as E
import Control.Monad.IO.Class (liftIO)
import Data.List (isInfixOf)
import System.Exit (ExitCode (..))
import System.Process (CreateProcess (create_group), proc, readCreateProcessWithExitCode, readProcessWithExitCode)
import Text.Printf (printf)
import Wasp.Cli.Command (Command, CommandError (CommandError))
import Wasp.Cli.Command.Common (throwIfExeIsNotAvailable)
import Wasp.Cli.Command.Message (cliSendMessageC)
import qualified Wasp.Job as Job
import Wasp.Job.IO.PrefixedWriter (printJobMessagePrefixed, runPrefixedWriter)
import Wasp.Job.Process (runProcessAsJob)
import qualified Wasp.Message as Msg
import Wasp.Util.Docker (DockerImageName)

ensureDockerDaemonAvailable :: Command ()
ensureDockerDaemonAvailable = do
  throwIfExeIsNotAvailable
    "docker"
    "To run PostgreSQL dev database, Wasp needs `docker` installed and in PATH."
  (exitCode, _, stderr) <- liftIO $ readProcessWithExitCode "docker" ["info", "--format", "{{.ServerVersion}}"] ""
  when (exitCode /= ExitSuccess) $ E.throwError $ CommandError "Docker unavailable" (printf "Start Docker and retry. %s" stderr)

ensureImageAvailable :: DockerImageName -> Command ()
ensureImageAvailable dbDockerImage = do
  (imageStatus, _, _) <- liftIO $ readProcessWithExitCode "docker" ["image", "inspect", dbDockerImage] ""
  when (imageStatus /= ExitSuccess) $ do
    cliSendMessageC $ Msg.Start $ printf "Pulling PostgreSQL image %s..." dbDockerImage
    pullStatus <- liftIO $ runDockerCommand ["pull", dbDockerImage]
    when (pullStatus /= ExitSuccess) $ E.throwError $ CommandError "Could not pull PostgreSQL image" (printf "Check the image name, registry access, and network connection. Image: %s" dbDockerImage)

runDockerCommand :: [String] -> IO ExitCode
runDockerCommand args = do
  channel <- newChan
  fst <$> concurrently (runProcessAsJob (proc "docker" args) Job.Db channel) (printDbMessages channel)

printDbMessages :: Chan Job.JobMessage -> IO ()
printDbMessages channel = runPrefixedWriter go
  where
    go = do
      message <- liftIO $ readChan channel
      case Job._data message of
        Job.JobOutput output _ -> do
          printJobMessagePrefixed $ Job.JobMessage (Job.JobOutput output Job.Stdout) Job.Db
          go
        Job.JobExit _ -> return ()

stopDatabase :: String -> Command ()
stopDatabase containerId = do
  cliSendMessageC $ Msg.Start "Stopping database..."
  failure <- runDockerCleanup ["stop", containerId]
  forM_ failure $ \errors ->
    cliSendMessageC $ Msg.Warning "Could not stop PostgreSQL" (printf "Wasp will try to remove the container. %s" errors)

removeContainer :: String -> Command ()
removeContainer containerId = do
  failure <- runDockerCleanup ["rm", "--force", containerId]
  forM_ failure $ \errors ->
    cliSendMessageC $ Msg.Warning "Could not remove PostgreSQL container" (printf "Try `docker rm --force %s`. %s" containerId errors)

runDockerCleanup :: [String] -> Command (Maybe String)
runDockerCleanup args = do
  -- create_group keeps terminal Ctrl+C from interrupting Docker's cleanup command.
  (status, _, errors) <- liftIO $ readCreateProcessWithExitCode ((proc "docker" args) {create_group = True}) ""
  return $
    if status == ExitSuccess || "No such container" `isInfixOf` errors
      then Nothing
      else Just errors
