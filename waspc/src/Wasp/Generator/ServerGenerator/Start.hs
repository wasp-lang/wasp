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
runServerProcessController serverDir controller =
  J.makeJob J.Server $ \outputSink -> do
    -- Only the controller thread (this one) reads and writes these refs,
    -- including the 'finally' cleanup below. The process exit watchers spawned
    -- in 'startServerProcess' only write commands to the controller channel.
    serverStateRef <- newIORef ServerNotRunning
    nextServerProcessIdRef <- newIORef 0
    runServerProcessControllerLoop serverDir controller serverStateRef nextServerProcessIdRef outputSink
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
  J.JobOutputSink ->
  IO ()
runServerProcessControllerLoop serverDir controller serverStateRef nextServerProcessIdRef outputSink = do
  handleSuccessfulCompile serverDir controller serverStateRef nextServerProcessIdRef outputSink RebundleAndRestartServer
  processServerCommands
  where
    processServerCommands = do
      command <- readServerControllerCommand controller
      case command of
        SuccessfulCompile serverEffect done ->
          processBlockingCommand done $
            handleSuccessfulCompile serverDir controller serverStateRef nextServerProcessIdRef outputSink serverEffect
        FailedCompile done ->
          processBlockingCommand done $
            stopServerFromStateRef serverStateRef
        ServerProcessExited serverProcessId exitCode ->
          handleServerProcessExited serverStateRef outputSink serverProcessId exitCode
      processServerCommands

    processBlockingCommand done action = action `finally` putMVar done ()

handleSuccessfulCompile ::
  Path' Abs (Dir ServerRootDir) ->
  ServerProcessController ->
  IORef ServerProcessState ->
  IORef Int ->
  J.JobOutputSink ->
  ServerEffect ->
  IO ()
handleSuccessfulCompile serverDir controller serverStateRef nextServerProcessIdRef outputSink serverEffect = do
  reconcileExitedServerProcess serverStateRef outputSink
  serverState <- readIORef serverStateRef
  case (serverState, serverEffect) of
    (ServerRunning {}, NoServerEffect) -> return ()
    (ServerRunning {}, RestartServer) ->
      replaceServerProcess serverDir controller serverStateRef nextServerProcessIdRef outputSink
    _ -> do
      bundleExitCode <- bundleServer serverDir outputSink
      case bundleExitCode of
        ExitSuccess -> replaceServerProcess serverDir controller serverStateRef nextServerProcessIdRef outputSink
        ExitFailure {} -> stopServerFromStateRef serverStateRef

replaceServerProcess ::
  Path' Abs (Dir ServerRootDir) ->
  ServerProcessController ->
  IORef ServerProcessState ->
  IORef Int ->
  J.JobOutputSink ->
  IO ()
replaceServerProcess serverDir controller serverStateRef nextServerProcessIdRef outputSink = do
  stopServerFromStateRef serverStateRef
  startServerProcess serverDir controller serverStateRef nextServerProcessIdRef outputSink

bundleServer :: Path' Abs (Dir ServerRootDir) -> J.JobOutputSink -> IO ExitCode
bundleServer serverDir =
  runNodeCommandAndStreamOutputWithExtraEnv [] serverDir "npm" ["run", "bundle"]

startServerProcess ::
  Path' Abs (Dir ServerRootDir) ->
  ServerProcessController ->
  IORef ServerProcessState ->
  IORef Int ->
  J.JobOutputSink ->
  IO ()
startServerProcess serverDir controller serverStateRef nextServerProcessIdRef outputSink =
  makeNodeCommandProcessWithExtraEnv [("NODE_ENV", "development")] serverDir Common.devServerStartExecutable Common.devServerStartArgs >>= \serverProcess -> mask_ $ do
    serverProcessId <- getNextServerProcessId nextServerProcessIdRef
    longRunningProcess <- LongRunning.start serverProcess outputSink
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

handleServerProcessExited :: IORef ServerProcessState -> J.JobOutputSink -> ServerProcessId -> ExitCode -> IO ()
handleServerProcessExited serverStateRef outputSink serverProcessId exitCode = do
  serverState <- readIORef serverStateRef
  case serverState of
    ServerRunning serverProcess
      | _serverProcessId serverProcess == serverProcessId ->
          cleanUpExitedServerProcess serverStateRef outputSink serverProcess exitCode
    _ -> return ()

reconcileExitedServerProcess :: IORef ServerProcessState -> J.JobOutputSink -> IO ()
reconcileExitedServerProcess serverStateRef outputSink = do
  serverState <- readIORef serverStateRef
  case serverState of
    ServerNotRunning -> return ()
    ServerRunning serverProcess ->
      LongRunning.pollRootExit (_longRunningProcess serverProcess) >>= \case
        Nothing -> return ()
        Just exitCode -> cleanUpExitedServerProcess serverStateRef outputSink serverProcess exitCode

cleanUpExitedServerProcess :: IORef ServerProcessState -> J.JobOutputSink -> ServerProcess -> ExitCode -> IO ()
cleanUpExitedServerProcess serverStateRef outputSink serverProcess exitCode = do
  -- The root process exited on its own, but its descendants may have survived
  -- and could still hold the server port or output pipes.
  LongRunning.stop $ _longRunningProcess serverProcess
  printServerProcessExit outputSink exitCode
  writeIORef serverStateRef ServerNotRunning

printServerProcessExit :: J.JobOutputSink -> ExitCode -> IO ()
printServerProcessExit outputSink exitCode =
  J.writeJobOutput outputSink (outputType exitCode) $ formatServerProcessExit exitCode
  where
    outputType ExitSuccess = J.Stdout
    outputType ExitFailure {} = J.Stderr

formatServerProcessExit :: ExitCode -> T.Text
formatServerProcessExit ExitSuccess = "Server process exited.\n"
formatServerProcessExit (ExitFailure exitCode) = T.pack $ "Server process exited with code " <> show exitCode <> ".\n"
