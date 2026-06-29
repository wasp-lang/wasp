module Wasp.Generator.ServerGenerator.Start
  ( ServerProcessCommandQueue,
    ServerUpdateImpact (..),
    newServerProcessCommandQueue,
    requestServerUpdate,
    requestServerStop,
    startServer,
  )
where

import Control.Concurrent.Async (Async, async, cancel, concurrently, poll, waitCatch)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket, finally)
import Control.Monad (forever, unless, void)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Text as T
import Data.Unique (Unique, newUnique)
import StrongPath (Abs, Dir, Path', (</>))
import System.Exit (ExitCode (..))
import Wasp.Generator.Common (GeneratedAppDir)
import qualified Wasp.Generator.ServerGenerator.Common as Common
import qualified Wasp.Job as J
import Wasp.Job.Process (runManagedNodeCommandAsJob, runNodeCommandAsJob)

newtype ServerProcessCommandQueue = ServerProcessCommandQueue (Chan ServerProcessCommandRequest)

data ServerProcessCommandRequest = ServerProcessCommandRequest
  { serverProcessCommand :: ServerProcessCommand,
    serverProcessCommandDone :: MVar ()
  }

data ServerProcessCommand
  = UpdateServer ServerUpdateImpact
  | StopServer

data ServerUpdateImpact
  = ServerMayBeAffected
  | ServerUnaffected

newServerProcessCommandQueue :: IO ServerProcessCommandQueue
newServerProcessCommandQueue = ServerProcessCommandQueue <$> newChan

requestServerUpdate :: ServerProcessCommandQueue -> ServerUpdateImpact -> IO ()
requestServerUpdate serverProcessCommandQueue serverUpdateImpact = requestServerProcessCommand (UpdateServer serverUpdateImpact) serverProcessCommandQueue

requestServerStop :: ServerProcessCommandQueue -> IO ()
requestServerStop = requestServerProcessCommand StopServer

requestServerProcessCommand :: ServerProcessCommand -> ServerProcessCommandQueue -> IO ()
requestServerProcessCommand command (ServerProcessCommandQueue queue) = do
  commandDone <- newEmptyMVar
  writeChan queue $ ServerProcessCommandRequest command commandDone
  takeMVar commandDone

startServer :: Path' Abs (Dir GeneratedAppDir) -> ServerProcessCommandQueue -> J.Job
startServer generatedAppDir serverProcessCommandQueue = runServerProcessManager serverDir serverProcessCommandQueue
  where
    serverDir = generatedAppDir </> Common.serverRootDirInGeneratedAppDir

type ServerRootDirAbs = Path' Abs (Dir Common.ServerRootDir)

data ServerProcessState
  = ServerNotRunning
  | ServerRunning ServerProcess

data ServerProcess = ServerProcess
  { serverProcessId :: Unique,
    serverJob :: Async ExitCode,
    serverOutputForwarder :: Async (),
    serverExitReporter :: Async (),
    serverStopRequested :: IORef Bool
  }

runServerProcessManager :: ServerRootDirAbs -> ServerProcessCommandQueue -> J.Job
runServerProcessManager serverDir serverProcessCommandQueue jobMessages =
  bracket (newIORef ServerNotRunning) stopRunningServerProcess $ \serverState -> do
    refreshServerProcess serverDir serverState jobMessages
    forever $ do
      commandRequest <- readServerProcessCommandRequest serverProcessCommandQueue
      runServerProcessCommand serverDir serverState jobMessages (serverProcessCommand commandRequest)
        `finally` confirmServerProcessCommand commandRequest

readServerProcessCommandRequest :: ServerProcessCommandQueue -> IO ServerProcessCommandRequest
readServerProcessCommandRequest (ServerProcessCommandQueue queue) = readChan queue

confirmServerProcessCommand :: ServerProcessCommandRequest -> IO ()
confirmServerProcessCommand request = putMVar (serverProcessCommandDone request) ()

runServerProcessCommand :: ServerRootDirAbs -> IORef ServerProcessState -> Chan J.JobMessage -> ServerProcessCommand -> IO ()
runServerProcessCommand serverDir serverState jobMessages (UpdateServer serverUpdateImpact) = updateServerProcess serverDir serverState jobMessages serverUpdateImpact
runServerProcessCommand _ serverState _ StopServer = stopRunningServerProcess serverState

updateServerProcess :: ServerRootDirAbs -> IORef ServerProcessState -> Chan J.JobMessage -> ServerUpdateImpact -> IO ()
updateServerProcess serverDir serverState jobMessages serverUpdateImpact = do
  currentServerProcess <- syncServerProcessState serverState
  if serverRefreshNeeded serverUpdateImpact currentServerProcess
    then refreshServerProcessWithState serverDir serverState jobMessages currentServerProcess
    else return ()

serverRefreshNeeded :: ServerUpdateImpact -> ServerProcessState -> Bool
serverRefreshNeeded ServerMayBeAffected _ = True
serverRefreshNeeded ServerUnaffected ServerNotRunning = True
serverRefreshNeeded ServerUnaffected (ServerRunning _) = False

refreshServerProcess :: ServerRootDirAbs -> IORef ServerProcessState -> Chan J.JobMessage -> IO ()
refreshServerProcess serverDir serverState jobMessages = do
  currentServerProcess <- syncServerProcessState serverState
  refreshServerProcessWithState serverDir serverState jobMessages currentServerProcess

refreshServerProcessWithState :: ServerRootDirAbs -> IORef ServerProcessState -> Chan J.JobMessage -> ServerProcessState -> IO ()
refreshServerProcessWithState serverDir serverState jobMessages currentServerProcess = do
  bundleSucceeded <- buildServerBundle serverDir jobMessages
  if bundleSucceeded
    then do
      stopServerProcessState serverState currentServerProcess
      startServerProcess serverState serverDir jobMessages
    else do
      stopServerProcessState serverState currentServerProcess
      reportServerBundleFailure currentServerProcess jobMessages

syncServerProcessState :: IORef ServerProcessState -> IO ServerProcessState
syncServerProcessState serverState = do
  state <- readIORef serverState
  case state of
    ServerNotRunning -> return ServerNotRunning
    ServerRunning serverProcess -> do
      serverJobStatus <- poll $ serverJob serverProcess
      case serverJobStatus of
        Nothing -> return state
        Just _ -> markServerProcessNotRunning serverState (serverProcessId serverProcess) >> readIORef serverState

markServerProcessNotRunning :: IORef ServerProcessState -> Unique -> IO ()
markServerProcessNotRunning serverState stoppedServerProcessId =
  atomicModifyIORef' serverState $ \state ->
    case state of
      ServerRunning serverProcess
        | serverProcessId serverProcess == stoppedServerProcessId -> (ServerNotRunning, ())
      _ -> (state, ())

buildServerBundle :: ServerRootDirAbs -> Chan J.JobMessage -> IO Bool
buildServerBundle serverDir jobMessages = do
  serverBundleMessages <- newChan
  (_, exitCode) <-
    concurrently
      (forwardJobOutputOnly serverBundleMessages jobMessages)
      (runNodeCommandAsJob serverDir "npm" ["run", "bundle"] J.Server serverBundleMessages)
  return $ exitCode == ExitSuccess

reportServerBundleFailure :: ServerProcessState -> Chan J.JobMessage -> IO ()
reportServerBundleFailure ServerNotRunning jobMessages =
  writeServerMessage jobMessages J.Stderr "Server bundle failed. Server is not running.\n"
reportServerBundleFailure (ServerRunning _) jobMessages =
  writeServerMessage jobMessages J.Stderr "Server bundle failed. Server stopped.\n"

startServerProcess :: IORef ServerProcessState -> ServerRootDirAbs -> Chan J.JobMessage -> IO ()
startServerProcess serverState serverDir jobMessages = do
  processId <- newUnique
  serverMessages <- newChan
  stopRequested <- newIORef False
  serverJobThread <- async $ runManagedNodeCommandAsJob [("NODE_ENV", "development")] serverDir "npm" ["run", "start"] J.Server serverMessages
  outputForwarder <- async $ forwardJobOutputOnly serverMessages jobMessages
  exitReporter <- async $ reportServerExit serverState processId serverJobThread outputForwarder stopRequested jobMessages
  let serverProcess =
        ServerProcess
          { serverProcessId = processId,
            serverJob = serverJobThread,
            serverOutputForwarder = outputForwarder,
            serverExitReporter = exitReporter,
            serverStopRequested = stopRequested
          }
  writeIORef serverState $ ServerRunning serverProcess
  void $ syncServerProcessState serverState

reportServerExit :: IORef ServerProcessState -> Unique -> Async ExitCode -> Async () -> IORef Bool -> Chan J.JobMessage -> IO ()
reportServerExit serverState processId serverJob outputForwarder stopRequested jobMessages = do
  exitResult <- waitCatch serverJob
  markServerProcessNotRunning serverState processId
  void $ waitCatch outputForwarder
  wasStopRequested <- readIORef stopRequested
  unless wasStopRequested $
    case exitResult of
      Right ExitSuccess ->
        writeServerMessage jobMessages J.Stdout "Server process exited. Waiting for the next successful compile to start it again.\n"
      Right (ExitFailure exitCode) ->
        writeServerMessage jobMessages J.Stderr $
          "Server process exited with code " <> show exitCode <> ". Waiting for the next successful compile to start it again.\n"
      Left _ -> return ()

stopRunningServerProcess :: IORef ServerProcessState -> IO ()
stopRunningServerProcess serverState =
  syncServerProcessState serverState >>= stopServerProcessState serverState

stopServerProcessState :: IORef ServerProcessState -> ServerProcessState -> IO ()
stopServerProcessState _ ServerNotRunning = return ()
stopServerProcessState serverState (ServerRunning serverProcess) = do
  stopServerProcess serverProcess
  markServerProcessNotRunning serverState $ serverProcessId serverProcess

stopServerProcess :: ServerProcess -> IO ()
stopServerProcess serverProcess = do
  writeIORef (serverStopRequested serverProcess) True
  cancel $ serverJob serverProcess
  cancel $ serverOutputForwarder serverProcess
  cancel $ serverExitReporter serverProcess

forwardJobOutputOnly :: Chan J.JobMessage -> Chan J.JobMessage -> IO ()
forwardJobOutputOnly source target = do
  jobMessage <- readChan source
  case J._data jobMessage of
    J.JobOutput {} -> writeChan target jobMessage >> forwardJobOutputOnly source target
    J.JobExit {} -> return ()

writeServerMessage :: Chan J.JobMessage -> J.JobOutputType -> String -> IO ()
writeServerMessage jobMessages outputType message =
  writeChan jobMessages $
    J.JobMessage
      { J._data = J.JobOutput (T.pack message) outputType,
        J._jobType = J.Server
      }
