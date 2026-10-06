module Wasp.Job.Process
  ( ProcessGroupDidNotStop (..),
    Subprocess,
    command,
    interactive,
    run,
    run_,
    spawn,
    wait,
    poll,
    stop,
  )
where

import qualified Control.Concurrent.Async as Async
import Control.Exception (Exception (displayException), SomeException, finally, onException, throwIO, try)
import Control.Monad (unless, void)
import qualified Control.Monad.Catch as Catch
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (ReleaseKey, allocate, release)
import Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import System.Exit (ExitCode)
import System.IO (Handle, hClose)
import qualified System.Process as P
import System.Timeout (timeout)
import Wasp.Job (Job, Sink, Stream (..), getSink, requireExitSuccess)
import qualified Wasp.Process.System as System
import Wasp.Util (secondsToMicroSeconds)

-- TODO(#4575):
--   Switch from System.Process to System.Process.Typed.

data ProcessGroupDidNotStop = ProcessGroupDidNotStop
  deriving (Show, Eq)

instance Exception ProcessGroupDidNotStop where
  displayException _ = "Could not stop the subprocess group. A child process may still be running."

-- | A command with an empty stdin. It runs in its own process group, so
-- stopping it also stops every process it started.
command :: FilePath -> [String] -> P.CreateProcess
command executable arguments = (P.proc executable arguments) {P.std_in = P.CreatePipe}

-- | Lets the command read from Wasp's terminal. For that, it has to stay in
-- Wasp's process group, so stopping it only stops its root process.
interactive :: P.CreateProcess -> P.CreateProcess
interactive process = process {P.std_in = P.Inherit}

-- | Runs the process to completion and forwards all its output to the job's
-- sink before returning. See 'spawn' for how the process is started and
-- stopped.
run :: P.CreateProcess -> Job ExitCode
run process = Catch.bracket (spawn process) stop wait

-- | Like 'run', but fails the job if the process exits with a nonzero code.
run_ :: P.CreateProcess -> Job ()
run_ process = run process >>= requireExitSuccess

-- | A process started with 'spawn'.
data Subprocess = Subprocess
  { _rootExit :: Async.Async ExitCode,
    _stopKey :: ReleaseKey
  }

-- | Starts the process and forwards its output to the job's sink in the
-- background. The job stops the process when it finishes, unless it was
-- stopped before with 'stop'.
--
-- A process that doesn't inherit Wasp's stdin (see 'interactive') runs in its
-- own process group, and stopping it stops every process left in the group,
-- even after the root process has exited. A process whose stdin is
-- 'P.CreatePipe' gets an empty stdin.
spawn :: P.CreateProcess -> Job Subprocess
spawn process = do
  sink <- getSink
  (stopKey, startedProcess) <- allocate (startProcess sink process) stopProcess
  return $ Subprocess (_startedRootExit startedProcess) stopKey

-- | Waits for the root process to exit. Other processes in its group may still
-- be running until it is stopped.
wait :: (MonadIO m) => Subprocess -> m ExitCode
wait subprocess = liftIO $ Async.wait $ _rootExit subprocess

poll :: (MonadIO m) => Subprocess -> m (Maybe ExitCode)
poll subprocess =
  liftIO $
    Async.poll (_rootExit subprocess) >>= \case
      Nothing -> return Nothing
      Just (Left exception) -> throwIO exception
      Just (Right exitCode) -> return $ Just exitCode

-- | Stops the process, and forwards the output it wrote until then. Does
-- nothing if it was already stopped. Throws 'ProcessGroupDidNotStop' if its
-- process group doesn't stop in time.
stop :: (MonadIO m) => Subprocess -> m ()
stop = release . _stopKey

data StartedProcess = StartedProcess
  { _startedHandles :: ProcessHandles,
    _startedIsInteractive :: Bool,
    _startedProcessGroup :: Maybe P.Pid,
    _startedRootExit :: Async.Async ExitCode,
    _startedOutputForwarders :: [Async.Async ()]
  }

type ProcessHandles = (Maybe Handle, Maybe Handle, Maybe Handle, P.ProcessHandle)

startProcess :: Sink -> P.CreateProcess -> IO StartedProcess
startProcess sink process = do
  handles@(stdinHandle, stdoutHandle, stderrHandle, processHandle) <- P.createProcess configuredProcess
  ( do
      processGroup <- if isInteractive then return Nothing else P.getPid processHandle
      mapM_ hClose stdinHandle
      rootExit <- Async.async $ P.waitForProcess processHandle
      outputForwarders <-
        mapM
          Async.async
          [ forwardOutput sink Stdout stdoutHandle,
            forwardOutput sink Stderr stderrHandle
          ]
      return
        StartedProcess
          { _startedHandles = handles,
            _startedIsInteractive = isInteractive,
            _startedProcessGroup = processGroup,
            _startedRootExit = rootExit,
            _startedOutputForwarders = outputForwarders
          }
    )
    `onException` emergencyCleanUp isInteractive handles
  where
    isInteractive = P.std_in process == P.Inherit

    configuredProcess =
      if isInteractive
        then
          process
            { P.create_group = False,
              P.use_process_jobs = False,
              P.std_out = P.CreatePipe,
              P.std_err = P.CreatePipe
            }
        else System.configureIsolatedProcess process

stopProcess :: StartedProcess -> IO ()
stopProcess startedProcess = do
  stopped <- stopProcessTree `finally` cleanUpOutput
  unless stopped $ throwIO ProcessGroupDidNotStop
  where
    stopProcessTree =
      if _startedIsInteractive startedProcess
        then do
          P.getProcessExitCode processHandle >>= \case
            Just _ -> return ()
            Nothing -> P.terminateProcess processHandle
          return True
        else
          System.stopProcessGroup
            processHandle
            (_startedRootExit startedProcess)
            (_startedProcessGroup startedProcess)

    -- Processes that escaped the group could keep the output pipes open, so
    -- we don't wait for the output forever.
    cleanUpOutput =
      mapM_ drainOrCancel (_startedOutputForwarders startedProcess)
        `finally` closeHandles (_startedHandles startedProcess)

    (_, _, _, processHandle) = _startedHandles startedProcess

drainOrCancel :: Async.Async () -> IO ()
drainOrCancel outputForwarder =
  timeout outputDrainTimeoutMicroseconds (Async.waitCatch outputForwarder) >>= \case
    Nothing -> Async.cancel outputForwarder
    Just (Left exception) -> throwIO exception
    Just (Right ()) -> return ()

forwardOutput :: Sink -> Stream -> Maybe Handle -> IO ()
forwardOutput _ _ Nothing = return ()
forwardOutput sink stream (Just handle) =
  runConduit $
    CB.sourceHandle handle .| CT.decodeUtf8Lenient .| CL.mapM_ (sink stream)

emergencyCleanUp :: Bool -> ProcessHandles -> IO ()
emergencyCleanUp isInteractive handles@(_, _, _, processHandle) = do
  unless isInteractive $ System.killStartedProcessGroup =<< P.getPid processHandle
  ignoreExceptions $ P.terminateProcess processHandle
  closeHandles handles
  void $ timeout System.hardStopTimeoutMicroseconds $ ignoreExceptions $ P.waitForProcess processHandle

closeHandles :: ProcessHandles -> IO ()
closeHandles (stdinHandle, stdoutHandle, stderrHandle, _) =
  mapM_ (mapM_ $ ignoreExceptions . hClose) [stdinHandle, stdoutHandle, stderrHandle]

ignoreExceptions :: IO a -> IO ()
ignoreExceptions action = void (try (void action) :: IO (Either SomeException ()))

outputDrainTimeoutMicroseconds :: Int
outputDrainTimeoutMicroseconds = secondsToMicroSeconds 1
