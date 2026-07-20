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
import Control.Exception (finally, mask_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Text as T
import StrongPath (Abs, Dir, Path', (</>))
import System.Exit (ExitCode (..))
import Wasp.Generator.Common (GeneratedAppDir, ServerRootDir)
import qualified Wasp.Generator.ServerGenerator.Common as Common
import qualified Wasp.Job as J
import Wasp.Job.Node (makeNodeCommandProcessWithExtraEnv, runNodeCommandAndStreamOutputWithExtraEnv)
import qualified Wasp.Job.Process.LongRunning as LongRunning

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
    _longRunningProcess :: LongRunning.LongRunningProcess
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

startServer :: Path' Abs (Dir GeneratedAppDir) -> ServerProcessController -> J.Job
startServer generatedAppDir controller =
  runServerProcessController serverDir controller
  where
    serverDir = generatedAppDir </> Common.serverRootDirInGeneratedAppDir

runServerProcessController :: Path' Abs (Dir ServerRootDir) -> ServerProcessController -> J.Job
runServerProcessController serverDir controller chan = do
  -- Only the controller thread (this one) reads and writes these refs,
  -- including the 'finally' cleanup below. The process exit watchers spawned
  -- in 'startServerProcess' only write commands to the controller channel.
  serverStateRef <- newIORef ServerNotRunning
  nextServerProcessIdRef <- newIORef 0
  runServerProcessControllerLoop serverDir controller serverStateRef nextServerProcessIdRef chan
    `finally` stopServerFromStateRef serverStateRef
  return ExitSuccess

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
  Chan J.JobMessage ->
  IO ()
runServerProcessControllerLoop serverDir controller serverStateRef nextServerProcessIdRef chan = do
  handleSuccessfulCompile serverDir controller serverStateRef nextServerProcessIdRef chan RebundleAndRestartServer
  processServerCommands
  where
    processServerCommands = do
      command <- readServerControllerCommand controller
      case command of
        SuccessfulCompile serverEffect done ->
          processBlockingCommand done $
            handleSuccessfulCompile serverDir controller serverStateRef nextServerProcessIdRef chan serverEffect
        FailedCompile done ->
          processBlockingCommand done $
            stopServerFromStateRef serverStateRef
        ServerProcessExited serverProcessId exitCode ->
          handleServerProcessExited serverStateRef chan serverProcessId exitCode
      processServerCommands

    processBlockingCommand done action = action `finally` putMVar done ()

handleSuccessfulCompile ::
  Path' Abs (Dir ServerRootDir) ->
  ServerProcessController ->
  IORef ServerProcessState ->
  IORef Int ->
  Chan J.JobMessage ->
  ServerEffect ->
  IO ()
handleSuccessfulCompile serverDir controller serverStateRef nextServerProcessIdRef chan serverEffect = do
  reconcileExitedServerProcess serverStateRef chan
  serverState <- readIORef serverStateRef
  case (serverState, serverEffect) of
    (ServerRunning {}, NoServerEffect) -> return ()
    (ServerRunning {}, RestartServer) ->
      replaceServerProcess serverDir controller serverStateRef nextServerProcessIdRef chan
    _ -> do
      bundleExitCode <- bundleServer serverDir chan
      case bundleExitCode of
        ExitSuccess -> replaceServerProcess serverDir controller serverStateRef nextServerProcessIdRef chan
        ExitFailure {} -> stopServerFromStateRef serverStateRef

replaceServerProcess ::
  Path' Abs (Dir ServerRootDir) ->
  ServerProcessController ->
  IORef ServerProcessState ->
  IORef Int ->
  Chan J.JobMessage ->
  IO ()
replaceServerProcess serverDir controller serverStateRef nextServerProcessIdRef chan = do
  stopServerFromStateRef serverStateRef
  startServerProcess serverDir controller serverStateRef nextServerProcessIdRef chan

bundleServer :: Path' Abs (Dir ServerRootDir) -> J.JobOutputStreamer
bundleServer serverDir =
  runNodeCommandAndStreamOutputWithExtraEnv [] serverDir "npm" ["run", "bundle"] J.Server

startServerProcess ::
  Path' Abs (Dir ServerRootDir) ->
  ServerProcessController ->
  IORef ServerProcessState ->
  IORef Int ->
  Chan J.JobMessage ->
  IO ()
startServerProcess serverDir controller serverStateRef nextServerProcessIdRef chan =
  makeNodeCommandProcessWithExtraEnv [("NODE_ENV", "development")] serverDir Common.devServerStartExecutable Common.devServerStartArgs >>= \serverProcess -> mask_ $ do
    serverProcessId <- getNextServerProcessId nextServerProcessIdRef
    longRunningProcess <- LongRunning.start serverProcess J.Server chan
    writeIORef serverStateRef $ ServerRunning ServerProcess {_serverProcessId = serverProcessId, _longRunningProcess = longRunningProcess}
    exitWatcher <- async $ do
      exitCode <- LongRunning.waitForRootExit longRunningProcess
      writeServerControllerCommand controller $ ServerProcessExited serverProcessId exitCode
    link exitWatcher
    return ()

getNextServerProcessId :: IORef Int -> IO ServerProcessId
getNextServerProcessId nextServerProcessIdRef = do
  nextServerProcessId <- (+ 1) <$> readIORef nextServerProcessIdRef
  writeIORef nextServerProcessIdRef nextServerProcessId
  return $ ServerProcessId nextServerProcessId

stopServerFromStateRef :: IORef ServerProcessState -> IO ()
stopServerFromStateRef serverStateRef = mask_ $ do
  serverState <- readIORef serverStateRef
  case serverState of
    ServerNotRunning -> return ()
    ServerRunning serverProcess -> do
      LongRunning.stop $ _longRunningProcess serverProcess
      writeIORef serverStateRef ServerNotRunning

handleServerProcessExited :: IORef ServerProcessState -> Chan J.JobMessage -> ServerProcessId -> ExitCode -> IO ()
handleServerProcessExited serverStateRef chan serverProcessId exitCode = do
  serverState <- readIORef serverStateRef
  case serverState of
    ServerRunning serverProcess
      | _serverProcessId serverProcess == serverProcessId ->
          cleanUpExitedServerProcess serverStateRef chan serverProcess exitCode
    _ -> return ()

reconcileExitedServerProcess :: IORef ServerProcessState -> Chan J.JobMessage -> IO ()
reconcileExitedServerProcess serverStateRef chan = do
  serverState <- readIORef serverStateRef
  case serverState of
    ServerNotRunning -> return ()
    ServerRunning serverProcess ->
      LongRunning.pollRootExit (_longRunningProcess serverProcess) >>= \case
        Nothing -> return ()
        Just exitCode -> cleanUpExitedServerProcess serverStateRef chan serverProcess exitCode

cleanUpExitedServerProcess :: IORef ServerProcessState -> Chan J.JobMessage -> ServerProcess -> ExitCode -> IO ()
cleanUpExitedServerProcess serverStateRef chan serverProcess exitCode = do
  -- The root process exited on its own, but its descendants may have survived
  -- and could still hold the server port or output pipes.
  LongRunning.stop $ _longRunningProcess serverProcess
  printServerProcessExit chan exitCode
  writeIORef serverStateRef ServerNotRunning

printServerProcessExit :: Chan J.JobMessage -> ExitCode -> IO ()
printServerProcessExit chan exitCode =
  writeServerOutput chan (outputType exitCode) $ formatServerProcessExit exitCode
  where
    outputType ExitSuccess = J.Stdout
    outputType ExitFailure {} = J.Stderr

formatServerProcessExit :: ExitCode -> T.Text
formatServerProcessExit ExitSuccess = "Server process exited.\n"
formatServerProcessExit (ExitFailure exitCode) = T.pack $ "Server process exited with code " <> show exitCode <> ".\n"

writeServerOutput :: Chan J.JobMessage -> J.JobOutputType -> T.Text -> IO ()
writeServerOutput chan outputType output = J.writeJobOutput J.Server outputType output chan
