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
import qualified Wasp.Job.Output.Event as Event
import qualified Wasp.Job.Process as JobProcess
import Wasp.Process (InputMode (NoInput))

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
    _subprocess :: JobProcess.Subprocess
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

startServer :: ServerRunConfig -> Path' Abs (Dir GeneratedAppDir) -> ServerProcessController -> Job.Job ()
startServer serverRunConfig generatedAppDir =
  runServerProcessController serverRunConfig serverDir
  where
    serverDir = generatedAppDir </> Common.serverRootDirInGeneratedAppDir

runServerProcessController :: ServerRunConfig -> Path' Abs (Dir ServerRootDir) -> ServerProcessController -> Job.Job ()
runServerProcessController serverRunConfig serverDir controller = do
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
  Job.Job ()
runServerProcessControllerLoop serverRunConfig serverDir controller serverStateRef nextServerProcessIdRef = do
  handleSuccessfulCompile RebundleAndRestartServer
  processServerCommands
  where
    processServerCommands :: Job.Job ()
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

    handleSuccessfulCompile :: ServerEffect -> Job.Job ()
    handleSuccessfulCompile serverEffect = do
      reconcileExitedServerProcess
      serverState <- liftIO $ readIORef serverStateRef
      case (serverState, serverEffect) of
        (ServerRunning {}, NoServerEffect) -> return ()
        (ServerRunning {}, RestartServer) -> replaceServerProcess
        _ -> do
          bundleExitCode <- Node.runReturningExitCode NoInput [] serverDir "npm" ["run", "bundle"]
          case bundleExitCode of
            ExitSuccess -> replaceServerProcess
            ExitFailure {} -> stopServerProcess

    replaceServerProcess :: Job.Job ()
    replaceServerProcess = stopServerProcess >> startServerProcess

    startServerProcess :: Job.Job ()
    startServerProcess = do
      mask_ $ do
        serverProcessId <- liftIO getNextServerProcessId
        subprocess <- Node.spawn (("NODE_ENV", "development") : getEnvVars serverRunConfig) serverDir Common.devServerStartExecutable Common.devServerStartArgs
        liftIO $ writeIORef serverStateRef $ ServerRunning ServerProcess {_serverProcessId = serverProcessId, _subprocess = subprocess}
        exitWatcher <- liftIO $ async $ do
          exitCode <- JobProcess.wait subprocess
          writeServerControllerCommand controller $ ServerProcessExited serverProcessId exitCode
        liftIO $ link exitWatcher

    getNextServerProcessId :: IO ServerProcessId
    getNextServerProcessId = do
      nextServerProcessId <- (+ 1) <$> readIORef nextServerProcessIdRef
      writeIORef nextServerProcessIdRef nextServerProcessId
      return $ ServerProcessId nextServerProcessId

    stopServerProcess :: Job.Job ()
    stopServerProcess = mask_ $ do
      serverState <- liftIO $ readIORef serverStateRef
      case serverState of
        ServerNotRunning -> return ()
        ServerRunning serverProcess -> do
          JobProcess.stop $ _subprocess serverProcess
          liftIO $ writeIORef serverStateRef ServerNotRunning

    handleServerProcessExited :: ServerProcessId -> ExitCode -> Job.Job ()
    handleServerProcessExited serverProcessId exitCode = do
      serverState <- liftIO $ readIORef serverStateRef
      case serverState of
        ServerRunning serverProcess
          | _serverProcessId serverProcess == serverProcessId ->
              cleanUpExitedServerProcess serverProcess exitCode
        _ -> return ()

    reconcileExitedServerProcess :: Job.Job ()
    reconcileExitedServerProcess = do
      serverState <- liftIO $ readIORef serverStateRef
      case serverState of
        ServerNotRunning -> return ()
        ServerRunning serverProcess ->
          liftIO (JobProcess.poll $ _subprocess serverProcess) >>= \case
            Nothing -> return ()
            Just exitCode -> cleanUpExitedServerProcess serverProcess exitCode

    cleanUpExitedServerProcess :: ServerProcess -> ExitCode -> Job.Job ()
    cleanUpExitedServerProcess serverProcess exitCode = do
      -- The root process exited on its own, but its descendants may have survived
      -- and could still hold the server port or output pipes.
      JobProcess.stop $ _subprocess serverProcess
      printServerProcessExit exitCode
      liftIO $ writeIORef serverStateRef ServerNotRunning

printServerProcessExit :: ExitCode -> Job.Job ()
printServerProcessExit exitCode =
  Job.emitJobOutput (outputStream exitCode) $ formatServerProcessExit exitCode
  where
    outputStream ExitSuccess = Event.Stdout
    outputStream ExitFailure {} = Event.Stderr

formatServerProcessExit :: ExitCode -> T.Text
formatServerProcessExit ExitSuccess = "Server process exited.\n"
formatServerProcessExit (ExitFailure exitCode) = T.pack $ "Server process exited with code " <> show exitCode <> ".\n"
