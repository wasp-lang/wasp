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
import Wasp.Generator.Common (GeneratedAppDir, ServerRootDir)
import qualified Wasp.Generator.ServerGenerator.Common as Common
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

startServer :: Path' Abs (Dir GeneratedAppDir) -> ServerProcessController -> Job.Job
startServer generatedAppDir =
  runServerProcessController serverDir
  where
    serverDir = generatedAppDir </> Common.serverRootDirInGeneratedAppDir

runServerProcessController :: Path' Abs (Dir ServerRootDir) -> ServerProcessController -> Job.Job
runServerProcessController serverDir controller =
  Job.makeJob Job.Server $ do
    -- Only the controller thread (this one) reads and writes these refs,
    -- including the 'finally' cleanup below. The process exit watchers spawned
    -- in 'startServerProcess' only write commands to the controller channel.
    serverStateRef <- liftIO $ newIORef ServerNotRunning
    nextServerProcessIdRef <- liftIO $ newIORef 0
    runServerProcessControllerLoop serverDir controller serverStateRef nextServerProcessIdRef
      `finally` stopServerFromStateRef serverStateRef

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
  Path' Abs (Dir ServerRootDir) ->
  ServerProcessController ->
  IORef ServerProcessState ->
  IORef Int ->
  Job.JobAction ()
runServerProcessControllerLoop serverDir controller serverStateRef nextServerProcessIdRef = do
  handleSuccessfulCompile serverDir controller serverStateRef nextServerProcessIdRef RebundleAndRestartServer
  processServerCommands
  where
    processServerCommands = do
      command <- liftIO $ readServerControllerCommand controller
      case command of
        SuccessfulCompile serverEffect done ->
          processBlockingCommand done $
            handleSuccessfulCompile serverDir controller serverStateRef nextServerProcessIdRef serverEffect
        FailedCompile done ->
          processBlockingCommand done $
            stopServerFromStateRef serverStateRef
        ServerProcessExited serverProcessId exitCode ->
          handleServerProcessExited serverStateRef serverProcessId exitCode
      processServerCommands

    processBlockingCommand done action = action `finally` liftIO (putMVar done ())

handleSuccessfulCompile ::
  Path' Abs (Dir ServerRootDir) ->
  ServerProcessController ->
  IORef ServerProcessState ->
  IORef Int ->
  ServerEffect ->
  Job.JobAction ()
handleSuccessfulCompile serverDir controller serverStateRef nextServerProcessIdRef serverEffect = do
  reconcileExitedServerProcess serverStateRef
  serverState <- liftIO $ readIORef serverStateRef
  case (serverState, serverEffect) of
    (ServerRunning {}, NoServerEffect) -> return ()
    (ServerRunning {}, RestartServer) ->
      replaceServerProcess serverDir controller serverStateRef nextServerProcessIdRef
    _ -> do
      bundleExitCode <- bundleServer serverDir
      case bundleExitCode of
        ExitSuccess -> replaceServerProcess serverDir controller serverStateRef nextServerProcessIdRef
        ExitFailure {} -> stopServerFromStateRef serverStateRef

replaceServerProcess ::
  Path' Abs (Dir ServerRootDir) ->
  ServerProcessController ->
  IORef ServerProcessState ->
  IORef Int ->
  Job.JobAction ()
replaceServerProcess serverDir controller serverStateRef nextServerProcessIdRef = do
  stopServerFromStateRef serverStateRef
  startServerProcess serverDir controller serverStateRef nextServerProcessIdRef

bundleServer :: Path' Abs (Dir ServerRootDir) -> Job.JobAction ExitCode
bundleServer serverDir = Node.runReturningExitCode serverDir "npm" ["run", "bundle"]

startServerProcess ::
  Path' Abs (Dir ServerRootDir) ->
  ServerProcessController ->
  IORef ServerProcessState ->
  IORef Int ->
  Job.JobAction ()
startServerProcess serverDir controller serverStateRef nextServerProcessIdRef = do
  createProcess <- liftIO $ Node.makeCreateProcessWithExtraEnv [("NODE_ENV", "development")] serverDir Common.devServerStartExecutable Common.devServerStartArgs
  mask_ $ do
    serverProcessId <- liftIO $ getNextServerProcessId nextServerProcessIdRef
    subprocess <- Subprocess.spawn createProcess
    liftIO $ writeIORef serverStateRef $ ServerRunning ServerProcess {_serverProcessId = serverProcessId, _subprocess = subprocess}
    exitWatcher <- liftIO $ async $ do
      exitCode <- Subprocess.wait subprocess
      writeServerControllerCommand controller $ ServerProcessExited serverProcessId exitCode
    liftIO $ link exitWatcher
    return ()

getNextServerProcessId :: IORef Int -> IO ServerProcessId
getNextServerProcessId nextServerProcessIdRef = do
  nextServerProcessId <- (+ 1) <$> readIORef nextServerProcessIdRef
  writeIORef nextServerProcessIdRef nextServerProcessId
  return $ ServerProcessId nextServerProcessId

stopServerFromStateRef :: IORef ServerProcessState -> Job.JobAction ()
stopServerFromStateRef serverStateRef = mask_ $ do
  serverState <- liftIO $ readIORef serverStateRef
  case serverState of
    ServerNotRunning -> return ()
    ServerRunning serverProcess -> do
      Subprocess.stop $ _subprocess serverProcess
      liftIO $ writeIORef serverStateRef ServerNotRunning

handleServerProcessExited :: IORef ServerProcessState -> ServerProcessId -> ExitCode -> Job.JobAction ()
handleServerProcessExited serverStateRef serverProcessId exitCode = do
  serverState <- liftIO $ readIORef serverStateRef
  case serverState of
    ServerRunning serverProcess
      | _serverProcessId serverProcess == serverProcessId ->
          cleanUpExitedServerProcess serverStateRef serverProcess exitCode
    _ -> return ()

reconcileExitedServerProcess :: IORef ServerProcessState -> Job.JobAction ()
reconcileExitedServerProcess serverStateRef = do
  serverState <- liftIO $ readIORef serverStateRef
  case serverState of
    ServerNotRunning -> return ()
    ServerRunning serverProcess ->
      liftIO (Subprocess.poll $ _subprocess serverProcess) >>= \case
        Nothing -> return ()
        Just exitCode -> cleanUpExitedServerProcess serverStateRef serverProcess exitCode

cleanUpExitedServerProcess :: IORef ServerProcessState -> ServerProcess -> ExitCode -> Job.JobAction ()
cleanUpExitedServerProcess serverStateRef serverProcess exitCode = do
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
