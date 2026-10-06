module Wasp.Generator.ServerGenerator.Start
  ( ServerEffect (..),
    ServerProcessController,
    newServerProcessController,
    notifyFailedCompile,
    notifySuccessfulCompile,
    startServer,
  )
where

import Control.Concurrent (MVar, newEmptyMVar, takeMVar, tryPutMVar)
import Control.Concurrent.Async (wait, waitSTM, withAsync)
import Control.Concurrent.STM (STM, TQueue, atomically, newEmptyTMVarIO, newTQueueIO, orElse, putTMVar, readTMVar, readTQueue, writeTQueue)
import Control.Exception (onException)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import qualified Data.Text as T
import Data.Void (Void)
import StrongPath (Abs, Dir, Path', (</>))
import System.Exit (ExitCode (..))
import Wasp.Env (getEnvVars)
import Wasp.Generator.Common (GeneratedAppDir)
import qualified Wasp.Generator.ServerGenerator.Common as Common
import Wasp.Generator.ServerGenerator.RunConfig (ServerRunConfig (..))
import Wasp.Job (Job)
import qualified Wasp.Job as Job
import qualified Wasp.Job.Node as Node
import Wasp.Process (InputMode (NoInput), OutputStream (..))

newtype ServerProcessController = ServerProcessController (TQueue ServerControllerCommand)

-- Effect of a successful compile on a healthy, running server.
-- Without one, the controller conservatively rebundles before starting.
data ServerEffect
  = NoServerEffect
  | RestartServer
  | RebundleAndRestartServer
  deriving (Eq, Show)

instance Semigroup ServerEffect where
  NoServerEffect <> effect = effect
  effect <> NoServerEffect = effect
  RestartServer <> RestartServer = RestartServer
  _ <> _ = RebundleAndRestartServer

instance Monoid ServerEffect where
  mempty = NoServerEffect

data ServerControllerCommand
  = SuccessfulCompile ServerEffect Acknowledgement
  | FailedCompile Acknowledgement

-- | Filled once the server has been updated for the command, so the caller
-- can wait for it.
type Acknowledgement = MVar ()

data ServerStart = StartOnly | BundleAndStart

newServerProcessController :: IO ServerProcessController
newServerProcessController = ServerProcessController <$> newTQueueIO

notifySuccessfulCompile :: ServerProcessController -> ServerEffect -> IO ()
notifySuccessfulCompile controller serverEffect =
  sendServerControllerCommand controller $ SuccessfulCompile serverEffect

notifyFailedCompile :: ServerProcessController -> IO ()
notifyFailedCompile controller =
  sendServerControllerCommand controller FailedCompile

sendServerControllerCommand :: ServerProcessController -> (Acknowledgement -> ServerControllerCommand) -> IO ()
sendServerControllerCommand (ServerProcessController commands) makeCommand = do
  acknowledgement <- newEmptyMVar
  atomically $ writeTQueue commands $ makeCommand acknowledgement
  takeMVar acknowledgement

-- | Bundles and runs the development server, and keeps it up to date with the
-- compile results it is notified about until the job is stopped.
--
-- A running server is stopped before it is replaced, so the new one can take
-- over its port. If the server exits on its own, it stays stopped until the
-- next successful compile.
startServer :: ServerRunConfig -> Path' Abs (Dir GeneratedAppDir) -> ServerProcessController -> Job Void
startServer serverRunConfig generatedAppDir (ServerProcessController commands) =
  Job.fromCallback $ \emit -> runServer emit BundleAndStart Nothing
  where
    serverDir = generatedAppDir </> Common.serverRootDirInGeneratedAppDir

    runServer emit serverStart maybeAcknowledgement = do
      event <-
        runServerUntilUpdate emit (serverJob serverStart maybeAcknowledgement)
          `onException` traverse_ acknowledge maybeAcknowledgement
      case event of
        ServerJobEnded maybeExitCode -> do
          traverse_ (printServerProcessExit emit) maybeExitCode
          waitForCompile emit
        ServerUpdateRequested command -> handleCommandAfterStop emit command

    -- Runs the server job until it ends, or until a command requires stopping
    -- the server. Commands that don't affect the server are acknowledged
    -- right away.
    runServerUntilUpdate emit job = do
      stopRequest <- newEmptyTMVarIO
      withAsync (Job.runWith emit $ job $ readTMVar stopRequest) $ \runningJob ->
        let waitForEvent = do
              event <-
                atomically $
                  (ServerJobEnded <$> waitSTM runningJob)
                    `orElse` (ServerUpdateRequested <$> readTQueue commands)
              case event of
                ServerUpdateRequested (SuccessfulCompile NoServerEffect acknowledgement) ->
                  acknowledge acknowledgement >> waitForEvent
                ServerUpdateRequested _ -> do
                  atomically $ putTMVar stopRequest ()
                  void $ wait runningJob
                  return event
                ServerJobEnded _ -> return event
         in waitForEvent

    handleCommandAfterStop emit = \case
      FailedCompile acknowledgement -> acknowledge acknowledgement >> waitForCompile emit
      SuccessfulCompile RestartServer acknowledgement -> runServer emit StartOnly (Just acknowledgement)
      SuccessfulCompile _ acknowledgement -> runServer emit BundleAndStart (Just acknowledgement)

    -- No server is running, so any successful compile starts a freshly
    -- bundled one.
    waitForCompile emit =
      atomically (readTQueue commands) >>= \case
        FailedCompile acknowledgement -> acknowledge acknowledgement >> waitForCompile emit
        SuccessfulCompile _ acknowledgement -> runServer emit BundleAndStart (Just acknowledgement)

    -- Returns the server's exit code, or nothing if bundling failed.
    serverJob :: ServerStart -> Maybe Acknowledgement -> STM () -> Job (Maybe ExitCode)
    serverJob serverStart maybeAcknowledgement stopRequested = do
      bundleExitCode <- case serverStart of
        BundleAndStart -> Node.run NoInput [] serverDir "npm" ["run", "bundle"]
        StartOnly -> return ExitSuccess
      -- Any previous server has stopped and bundling is done, so the caller
      -- can continue while the new server starts.
      liftIO $ traverse_ acknowledge maybeAcknowledgement
      case bundleExitCode of
        ExitSuccess ->
          Just
            <$> Node.runUntil
              stopRequested
              NoInput
              (("NODE_ENV", "development") : getEnvVars serverRunConfig)
              serverDir
              Common.devServerStartExecutable
              Common.devServerStartArgs
        ExitFailure _ -> return Nothing

data ServerEvent
  = ServerJobEnded (Maybe ExitCode)
  | ServerUpdateRequested ServerControllerCommand

acknowledge :: Acknowledgement -> IO ()
acknowledge acknowledgement = void $ tryPutMVar acknowledgement ()

printServerProcessExit :: (OutputStream -> T.Text -> IO ()) -> ExitCode -> IO ()
printServerProcessExit emit exitCode =
  emit (outputStream exitCode) $ formatServerProcessExit exitCode
  where
    outputStream ExitSuccess = Stdout
    outputStream ExitFailure {} = Stderr

formatServerProcessExit :: ExitCode -> T.Text
formatServerProcessExit ExitSuccess = "Server process exited.\n"
formatServerProcessExit (ExitFailure exitCode) = T.pack $ "Server process exited with code " <> show exitCode <> ".\n"
