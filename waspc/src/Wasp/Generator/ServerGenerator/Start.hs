module Wasp.Generator.ServerGenerator.Start
  ( ServerEffect (..),
    ServerProcessController,
    newServerProcessController,
    notifyFailedCompile,
    notifySuccessfulCompile,
    startServer,
  )
where

import Control.Concurrent (Chan, MVar, newChan, newEmptyMVar, putMVar, readChan, takeMVar, writeChan)
import Control.Concurrent.Async (async, link)
import Control.Monad.Catch (finally, mask_)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Text as T
import StrongPath (Abs, Dir, Path', (</>))
import System.Exit (ExitCode (..))
import Wasp.Env (getEnvVars)
import Wasp.Generator.Common (GeneratedAppDir, ServerRootDir)
import qualified Wasp.Generator.ServerGenerator.Common as Common
import Wasp.Generator.ServerGenerator.RunConfig (ServerRunConfig (..))
import qualified Wasp.Job as Job
import qualified Wasp.Job.Node as Node
import qualified Wasp.Job.Subprocess as Subprocess

newtype ServerProcessController = ServerProcessController (Chan ServerControllerCommand)

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
  = SuccessfulCompile ServerEffect (MVar ())
  | FailedCompile (MVar ())
  | ServerProcessExited ServerProcessId ExitCode

newtype ServerProcessId = ServerProcessId Int deriving (Eq)

data ServerProcess = ServerProcess
  { _serverProcessId :: ServerProcessId,
    _subprocess :: Subprocess.Subprocess
  }

data ServerProcessState
  = ServerNotRunning
  | ServerRunning ServerProcess

newServerProcessController :: IO ServerProcessController
newServerProcessController = ServerProcessController <$> newChan

notifySuccessfulCompile :: ServerProcessController -> ServerEffect -> IO ()
notifySuccessfulCompile controller serverEffect =
  sendBlockingServerControllerCommand controller $ SuccessfulCompile serverEffect

notifyFailedCompile :: ServerProcessController -> IO ()
notifyFailedCompile controller =
  sendBlockingServerControllerCommand controller FailedCompile

startServer :: ServerRunConfig -> Path' Abs (Dir GeneratedAppDir) -> ServerProcessController -> Job.Job
startServer serverRunConfig generatedAppDir =
  runServerProcessController serverRunConfig serverDir
  where
    serverDir = generatedAppDir </> Common.serverRootDirInGeneratedAppDir

runServerProcessController :: ServerRunConfig -> Path' Abs (Dir ServerRootDir) -> ServerProcessController -> Job.Job
runServerProcessController serverRunConfig serverDir controller =
  Job.makeJob Job.Server $ do
    -- Only the controller thread accesses these refs. Exit watchers send commands.
    serverStateRef <- liftIO $ newIORef ServerNotRunning
    nextServerProcessIdRef <- liftIO $ newIORef 0
    runServerProcessControllerLoop serverRunConfig serverDir controller serverStateRef nextServerProcessIdRef

sendBlockingServerControllerCommand :: ServerProcessController -> (MVar () -> ServerControllerCommand) -> IO ()
sendBlockingServerControllerCommand (ServerProcessController commandChan) mkCommand = do
  done <- newEmptyMVar
  writeChan commandChan $ mkCommand done
  takeMVar done

readServerControllerCommand :: ServerProcessController -> IO ServerControllerCommand
readServerControllerCommand (ServerProcessController commandChan) = readChan commandChan

writeServerControllerCommand :: ServerProcessController -> ServerControllerCommand -> IO ()
writeServerControllerCommand (ServerProcessController commandChan) = writeChan commandChan

runServerProcessControllerLoop ::
  ServerRunConfig ->
  Path' Abs (Dir ServerRootDir) ->
  ServerProcessController ->
  IORef ServerProcessState ->
  IORef Int ->
  Job.JobAction ()
runServerProcessControllerLoop serverRunConfig serverDir controller serverStateRef nextServerProcessIdRef = do
  handleSuccessfulCompile RebundleAndRestartServer
  processServerCommands
  where
    processServerCommands :: Job.JobAction ()
    processServerCommands = do
      command <- liftIO $ readServerControllerCommand controller
      case command of
        SuccessfulCompile serverEffect done ->
          acknowledgeCommand done $ handleSuccessfulCompile serverEffect
        FailedCompile done ->
          acknowledgeCommand done stopServerProcess
        ServerProcessExited serverProcessId exitCode ->
          handleServerProcessExited serverProcessId exitCode
      processServerCommands

    acknowledgeCommand done action = action `finally` liftIO (putMVar done ())

    handleSuccessfulCompile :: ServerEffect -> Job.JobAction ()
    handleSuccessfulCompile serverEffect = do
      reconcileExitedServerProcess
      serverState <- liftIO $ readIORef serverStateRef
      case (serverState, serverEffect) of
        (ServerRunning {}, NoServerEffect) -> return ()
        (ServerRunning {}, RestartServer) -> replaceServerProcess
        _ -> do
          bundleExitCode <- Node.runReturningExitCode [] serverDir "npm" ["run", "bundle"]
          case bundleExitCode of
            ExitSuccess -> replaceServerProcess
            ExitFailure {} -> stopServerProcess

    replaceServerProcess :: Job.JobAction ()
    replaceServerProcess = stopServerProcess >> startServerProcess

    startServerProcess :: Job.JobAction ()
    startServerProcess = do
      createProcess <- liftIO $ Node.makeCreateProcess (("NODE_ENV", "development") : getEnvVars serverRunConfig) serverDir Common.devServerStartExecutable Common.devServerStartArgs
      mask_ $ do
        serverProcessId <- liftIO getNextServerProcessId
        subprocess <- Subprocess.spawn createProcess
        liftIO $ writeIORef serverStateRef $ ServerRunning ServerProcess {_serverProcessId = serverProcessId, _subprocess = subprocess}
        exitWatcher <- liftIO $ async $ do
          exitCode <- Subprocess.wait subprocess
          writeServerControllerCommand controller $ ServerProcessExited serverProcessId exitCode
        liftIO $ link exitWatcher

    getNextServerProcessId :: IO ServerProcessId
    getNextServerProcessId = do
      nextServerProcessId <- (+ 1) <$> readIORef nextServerProcessIdRef
      writeIORef nextServerProcessIdRef nextServerProcessId
      return $ ServerProcessId nextServerProcessId

    stopServerProcess :: Job.JobAction ()
    stopServerProcess = mask_ $ do
      serverState <- liftIO $ readIORef serverStateRef
      case serverState of
        ServerNotRunning -> return ()
        ServerRunning serverProcess -> do
          Subprocess.stop $ _subprocess serverProcess
          liftIO $ writeIORef serverStateRef ServerNotRunning

    handleServerProcessExited :: ServerProcessId -> ExitCode -> Job.JobAction ()
    handleServerProcessExited serverProcessId exitCode = do
      serverState <- liftIO $ readIORef serverStateRef
      case serverState of
        ServerRunning serverProcess
          | _serverProcessId serverProcess == serverProcessId ->
              cleanUpExitedServerProcess serverProcess exitCode
        _ -> return ()

    reconcileExitedServerProcess :: Job.JobAction ()
    reconcileExitedServerProcess = do
      serverState <- liftIO $ readIORef serverStateRef
      case serverState of
        ServerNotRunning -> return ()
        ServerRunning serverProcess ->
          liftIO (Subprocess.poll $ _subprocess serverProcess) >>= \case
            Nothing -> return ()
            Just exitCode -> cleanUpExitedServerProcess serverProcess exitCode

    cleanUpExitedServerProcess :: ServerProcess -> ExitCode -> Job.JobAction ()
    cleanUpExitedServerProcess serverProcess exitCode = do
      -- The root process exited on its own, but its descendants may have survived
      -- and could still hold the server port or output pipes.
      Subprocess.stop $ _subprocess serverProcess
      printServerProcessExit exitCode
      liftIO $ writeIORef serverStateRef ServerNotRunning

printServerProcessExit :: ExitCode -> Job.JobAction ()
printServerProcessExit exitCode =
  Job.emitJobOutput (outputStream exitCode) $ formatServerProcessExit exitCode
  where
    outputStream ExitSuccess = Job.Stdout
    outputStream ExitFailure {} = Job.Stderr

formatServerProcessExit :: ExitCode -> T.Text
formatServerProcessExit ExitSuccess = "Server process exited.\n"
formatServerProcessExit (ExitFailure exitCode) = T.pack $ "Server process exited with code " <> show exitCode <> ".\n"
