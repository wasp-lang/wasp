module Wasp.Generator.ServerGenerator.Start
  ( ServerChangeImpact (..),
    ServerProcessManager,
    newServerProcessManager,
    notifyFailedCompile,
    notifySuccessfulCompile,
    startServer,
  )
where

import Control.Concurrent (Chan, MVar, newChan, newEmptyMVar, putMVar, readChan, takeMVar, writeChan)
import Control.Concurrent.Async (async)
import Control.Exception (finally, mask_)
import Data.IORef (IORef, atomicModifyIORef', atomicWriteIORef, newIORef, readIORef)
import qualified Data.Text as T
import StrongPath (Abs, Dir, Path', (</>))
import System.Exit (ExitCode (..))
import Wasp.Generator.Common (GeneratedAppDir, ServerRootDir)
import qualified Wasp.Generator.ServerGenerator.Common as Common
import qualified Wasp.Job as J
import Wasp.Job.Node (makeNodeCommandProcessWithExtraEnv, runNodeCommandAndStreamOutputWithExtraEnv)
import Wasp.Job.Process.Managed
  ( ManagedProcess,
    getManagedProcessExitCode,
    startManagedProcess,
    stopManagedProcess,
    waitForManagedProcess,
  )

newtype ServerProcessManager = ServerProcessManager (Chan ServerManagerCommand)

data ServerChangeImpact
  = ServerMightBeAffected
  | ServerUnaffected

data ServerManagerCommand
  = SuccessfulCompile ServerChangeImpact (MVar ())
  | FailedCompile (MVar ())
  | ServerProcessExited ServerProcessId ExitCode

newtype ServerProcessId = ServerProcessId Int deriving (Eq)

data ServerProcess = ServerProcess
  { _serverProcessId :: ServerProcessId,
    _managedProcess :: ManagedProcess
  }

data ServerProcessState
  = ServerNotRunning
  | ServerRunning ServerProcess

newServerProcessManager :: IO ServerProcessManager
newServerProcessManager = ServerProcessManager <$> newChan

notifySuccessfulCompile :: ServerProcessManager -> ServerChangeImpact -> IO ()
notifySuccessfulCompile manager changeImpact =
  sendBlockingServerManagerCommand manager $ SuccessfulCompile changeImpact

notifyFailedCompile :: ServerProcessManager -> IO ()
notifyFailedCompile manager =
  sendBlockingServerManagerCommand manager FailedCompile

sendBlockingServerManagerCommand :: ServerProcessManager -> (MVar () -> ServerManagerCommand) -> IO ()
sendBlockingServerManagerCommand (ServerProcessManager commandChan) mkCommand = do
  done <- newEmptyMVar
  writeChan commandChan $ mkCommand done
  takeMVar done

readServerManagerCommand :: ServerProcessManager -> IO ServerManagerCommand
readServerManagerCommand (ServerProcessManager commandChan) = readChan commandChan

writeServerManagerCommand :: ServerProcessManager -> ServerManagerCommand -> IO ()
writeServerManagerCommand (ServerProcessManager commandChan) = writeChan commandChan

startServer :: Path' Abs (Dir GeneratedAppDir) -> ServerProcessManager -> J.Job
startServer generatedAppDir manager chan = do
  serverStateRef <- newIORef ServerNotRunning
  nextServerProcessIdRef <- newIORef 0
  let serverDir = generatedAppDir </> Common.serverRootDirInGeneratedAppDir
  runServerProcessManager serverDir manager serverStateRef nextServerProcessIdRef chan
    `finally` stopServerFromStateRef serverStateRef
  return ExitSuccess

runServerProcessManager ::
  Path' Abs (Dir ServerRootDir) ->
  ServerProcessManager ->
  IORef ServerProcessState ->
  IORef Int ->
  Chan J.JobMessage ->
  IO ()
runServerProcessManager serverDir manager serverStateRef nextServerProcessIdRef chan = do
  handleSuccessfulCompile serverDir manager serverStateRef nextServerProcessIdRef chan ServerMightBeAffected
  processServerCommands
  where
    processServerCommands = do
      command <- readServerManagerCommand manager
      case command of
        SuccessfulCompile changeImpact done ->
          processBlockingCommand done $
            handleSuccessfulCompile serverDir manager serverStateRef nextServerProcessIdRef chan changeImpact
        FailedCompile done ->
          processBlockingCommand done $
            stopServerFromStateRef serverStateRef
        ServerProcessExited serverProcessId exitCode ->
          handleServerProcessExited serverStateRef chan serverProcessId exitCode
      processServerCommands

    processBlockingCommand done action = action `finally` putMVar done ()

handleSuccessfulCompile ::
  Path' Abs (Dir ServerRootDir) ->
  ServerProcessManager ->
  IORef ServerProcessState ->
  IORef Int ->
  Chan J.JobMessage ->
  ServerChangeImpact ->
  IO ()
handleSuccessfulCompile serverDir manager serverStateRef nextServerProcessIdRef chan changeImpact = do
  syncServerState serverStateRef chan
  serverState <- readIORef serverStateRef
  case (serverState, changeImpact) of
    (ServerRunning {}, ServerUnaffected) -> return ()
    _ -> do
      bundleExitCode <- bundleServer serverDir chan
      case bundleExitCode of
        ExitSuccess -> do
          stopServerFromStateRef serverStateRef
          startServerProcess serverDir manager serverStateRef nextServerProcessIdRef chan
        ExitFailure {} -> stopServerFromStateRef serverStateRef

bundleServer :: Path' Abs (Dir ServerRootDir) -> Chan J.JobMessage -> IO ExitCode
bundleServer serverDir =
  runNodeCommandAndStreamOutputWithExtraEnv [] serverDir "npm" ["run", "bundle"] J.Server

startServerProcess ::
  Path' Abs (Dir ServerRootDir) ->
  ServerProcessManager ->
  IORef ServerProcessState ->
  IORef Int ->
  Chan J.JobMessage ->
  IO ()
startServerProcess serverDir manager serverStateRef nextServerProcessIdRef chan =
  makeNodeCommandProcessWithExtraEnv [("NODE_ENV", "development")] serverDir "npm" ["run", "start"] >>= \case
    Left errorMsg -> do
      writeServerOutput chan J.Stderr $ T.pack errorMsg
      atomicWriteIORef serverStateRef ServerNotRunning
    Right serverProcess -> mask_ $ do
      serverProcessId <- getNextServerProcessId nextServerProcessIdRef
      managedProcess <- startManagedProcess serverProcess J.Server chan
      atomicWriteIORef serverStateRef $ ServerRunning ServerProcess {_serverProcessId = serverProcessId, _managedProcess = managedProcess}
      _ <- async $ do
        exitCode <- waitForManagedProcess managedProcess
        writeServerManagerCommand manager $ ServerProcessExited serverProcessId exitCode
      return ()

getNextServerProcessId :: IORef Int -> IO ServerProcessId
getNextServerProcessId nextServerProcessIdRef =
  atomicModifyIORef' nextServerProcessIdRef $ \serverProcessId ->
    let nextServerProcessId = serverProcessId + 1
     in (nextServerProcessId, ServerProcessId nextServerProcessId)

stopServerFromStateRef :: IORef ServerProcessState -> IO ()
stopServerFromStateRef serverStateRef = mask_ $ do
  serverState <- readIORef serverStateRef
  case serverState of
    ServerNotRunning -> return ()
    ServerRunning serverProcess -> do
      stopManagedProcess $ _managedProcess serverProcess
      atomicWriteIORef serverStateRef ServerNotRunning

syncServerState :: IORef ServerProcessState -> Chan J.JobMessage -> IO ()
syncServerState serverStateRef chan = do
  serverState <- readIORef serverStateRef
  case serverState of
    ServerNotRunning -> return ()
    ServerRunning serverProcess ->
      getManagedProcessExitCode (_managedProcess serverProcess) >>= \case
        Nothing -> return ()
        Just exitCode -> do
          atomicWriteIORef serverStateRef ServerNotRunning
          printServerProcessExit chan exitCode

handleServerProcessExited :: IORef ServerProcessState -> Chan J.JobMessage -> ServerProcessId -> ExitCode -> IO ()
handleServerProcessExited serverStateRef chan serverProcessId exitCode = do
  serverState <- readIORef serverStateRef
  case serverState of
    ServerRunning serverProcess
      | _serverProcessId serverProcess == serverProcessId -> do
          atomicWriteIORef serverStateRef ServerNotRunning
          printServerProcessExit chan exitCode
    _ -> return ()

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
writeServerOutput chan outputType output =
  writeChan chan $
    J.JobMessage
      { J._data = J.JobOutput output outputType,
        J._jobType = J.Server
      }
