module Wasp.Generator.ServerGenerator.ServerProcessSupervisor
  ( ServerRuntimeInputChange (..),
    ServerProcessSupervisor,
    newServerProcessSupervisor,
    notifyFailedCompile,
    notifySuccessfulCompile,
    runServerProcessSupervisor,
  )
where

import Control.Concurrent (Chan, MVar, newChan, newEmptyMVar, putMVar, readChan, takeMVar, writeChan)
import Control.Concurrent.Async (async)
import Control.Exception (finally, mask_)
import Data.IORef (IORef, atomicModifyIORef', atomicWriteIORef, newIORef, readIORef)
import qualified Data.Text as T
import StrongPath (Abs, Dir, Path')
import System.Exit (ExitCode (..))
import Wasp.Generator.Common (ServerRootDir)
import qualified Wasp.Job as J
import Wasp.Job.Node (makeNodeCommandProcessWithExtraEnv, runNodeCommandAndStreamOutputWithExtraEnv)
import Wasp.Job.Process.Managed
  ( ManagedProcess,
    getManagedProcessExitCode,
    startManagedProcess,
    stopManagedProcess,
    waitForManagedProcess,
  )

newtype ServerProcessSupervisor = ServerProcessSupervisor (Chan ServerSupervisorCommand)

data ServerRuntimeInputChange
  = ServerRuntimeInputMightHaveChanged
  | NoServerRuntimeInputChange
  deriving (Eq, Show)

data ServerSupervisorCommand
  = SuccessfulCompile ServerRuntimeInputChange (MVar ())
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

newServerProcessSupervisor :: IO ServerProcessSupervisor
newServerProcessSupervisor = ServerProcessSupervisor <$> newChan

notifySuccessfulCompile :: ServerProcessSupervisor -> ServerRuntimeInputChange -> IO ()
notifySuccessfulCompile supervisor serverRuntimeInputChange =
  sendBlockingServerSupervisorCommand supervisor $ SuccessfulCompile serverRuntimeInputChange

notifyFailedCompile :: ServerProcessSupervisor -> IO ()
notifyFailedCompile supervisor =
  sendBlockingServerSupervisorCommand supervisor FailedCompile

runServerProcessSupervisor :: Path' Abs (Dir ServerRootDir) -> ServerProcessSupervisor -> J.Job
runServerProcessSupervisor serverDir supervisor chan = do
  serverStateRef <- newIORef ServerNotRunning
  nextServerProcessIdRef <- newIORef 0
  runServerProcessSupervisorLoop serverDir supervisor serverStateRef nextServerProcessIdRef chan
    `finally` stopServerFromStateRef serverStateRef
  return ExitSuccess

sendBlockingServerSupervisorCommand :: ServerProcessSupervisor -> (MVar () -> ServerSupervisorCommand) -> IO ()
sendBlockingServerSupervisorCommand (ServerProcessSupervisor commandChan) mkCommand = do
  done <- newEmptyMVar
  writeChan commandChan $ mkCommand done
  takeMVar done

readServerSupervisorCommand :: ServerProcessSupervisor -> IO ServerSupervisorCommand
readServerSupervisorCommand (ServerProcessSupervisor commandChan) = readChan commandChan

writeServerSupervisorCommand :: ServerProcessSupervisor -> ServerSupervisorCommand -> IO ()
writeServerSupervisorCommand (ServerProcessSupervisor commandChan) = writeChan commandChan

runServerProcessSupervisorLoop ::
  Path' Abs (Dir ServerRootDir) ->
  ServerProcessSupervisor ->
  IORef ServerProcessState ->
  IORef Int ->
  Chan J.JobMessage ->
  IO ()
runServerProcessSupervisorLoop serverDir supervisor serverStateRef nextServerProcessIdRef chan = do
  handleSuccessfulCompile serverDir supervisor serverStateRef nextServerProcessIdRef chan ServerRuntimeInputMightHaveChanged
  processServerCommands
  where
    processServerCommands = do
      command <- readServerSupervisorCommand supervisor
      case command of
        SuccessfulCompile serverRuntimeInputChange done ->
          processBlockingCommand done $
            handleSuccessfulCompile serverDir supervisor serverStateRef nextServerProcessIdRef chan serverRuntimeInputChange
        FailedCompile done ->
          processBlockingCommand done $
            stopServerFromStateRef serverStateRef
        ServerProcessExited serverProcessId exitCode ->
          handleServerProcessExited serverStateRef chan serverProcessId exitCode
      processServerCommands

    processBlockingCommand done action = action `finally` putMVar done ()

handleSuccessfulCompile ::
  Path' Abs (Dir ServerRootDir) ->
  ServerProcessSupervisor ->
  IORef ServerProcessState ->
  IORef Int ->
  Chan J.JobMessage ->
  ServerRuntimeInputChange ->
  IO ()
handleSuccessfulCompile serverDir supervisor serverStateRef nextServerProcessIdRef chan serverRuntimeInputChange = do
  syncServerState serverStateRef chan
  serverState <- readIORef serverStateRef
  case (serverState, serverRuntimeInputChange) of
    (ServerRunning {}, NoServerRuntimeInputChange) -> return ()
    _ -> do
      bundleExitCode <- bundleServer serverDir chan
      case bundleExitCode of
        ExitSuccess -> do
          stopServerFromStateRef serverStateRef
          startServerProcess serverDir supervisor serverStateRef nextServerProcessIdRef chan
        ExitFailure {} -> stopServerFromStateRef serverStateRef

bundleServer :: Path' Abs (Dir ServerRootDir) -> Chan J.JobMessage -> IO ExitCode
bundleServer serverDir =
  runNodeCommandAndStreamOutputWithExtraEnv [] serverDir "npm" ["run", "bundle"] J.Server

startServerProcess ::
  Path' Abs (Dir ServerRootDir) ->
  ServerProcessSupervisor ->
  IORef ServerProcessState ->
  IORef Int ->
  Chan J.JobMessage ->
  IO ()
startServerProcess serverDir supervisor serverStateRef nextServerProcessIdRef chan =
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
        writeServerSupervisorCommand supervisor $ ServerProcessExited serverProcessId exitCode
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
