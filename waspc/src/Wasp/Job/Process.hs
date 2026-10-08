module Wasp.Job.Process
  ( fromProc,
    fromInteractiveProc,
    ProcessGroupDidNotStop (..),
  )
where

import qualified Control.Concurrent.Async as Async
import Control.Exception (Exception (displayException), IOException, finally, mask, onException, throwIO, try)
import Control.Monad (unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import Data.Text (Text)
import System.Exit (ExitCode)
import System.IO (Handle, hClose)
import qualified System.Process as P
import System.Timeout (timeout)
import Wasp.Job.Common (Job (..), JobEnv (..), OutputType (..))
import qualified Wasp.Process.System as System

-- TODO(#4575):
--   Switch from System.Process to System.Process.Typed.

-- | Whether a process reads from Wasp's terminal.
data Interactivity = Isolated | Interactive

data ProcessGroupDidNotStop = ProcessGroupDidNotStop
  deriving (Show, Eq)

instance Exception ProcessGroupDidNotStop where
  displayException _ = "Could not stop the subprocess group. A child process may still be running."

-- | Runs the process to completion in its own process group (a job object on
-- Windows), with an empty stdin. Emits its stdout and stderr as the job's
-- output, and returns its exit code once all of it has been emitted.
--
-- Once the process exits, or the job is stopped, any process left in its group
-- is stopped too. If they don't stop in time, this throws
-- 'ProcessGroupDidNotStop'.
fromProc :: P.CreateProcess -> Job ExitCode
fromProc = runProcess Isolated

-- | Like 'fromProc', but the process reads from Wasp's stdin. For that, it
-- stays in Wasp's process group, so stopping the job only stops the process
-- itself, and not the processes it started.
fromInteractiveProc :: P.CreateProcess -> Job ExitCode
fromInteractiveProc = runProcess Interactive

runProcess :: Interactivity -> P.CreateProcess -> Job ExitCode
runProcess interactivity process = Job $ do
  env <- ask
  liftIO $ run interactivity (_sink env Nothing) (_processExitHook env) process

-- | Runs the process to completion, passing its output to the given function,
-- and returns its exit code once all of its output has been passed on. Calls
-- the given exit hook as soon as the process exits by itself.
--
-- An 'Isolated' process runs in its own process group (a job object on
-- Windows), with an empty stdin. Once it exits, or this is stopped, any
-- process left in its group is stopped too. If the group doesn't stop in time,
-- this throws 'ProcessGroupDidNotStop'.
--
-- An 'Interactive' process reads from Wasp's stdin. For that, it has to stay
-- in Wasp's process group, so stopping this only stops the process itself.
run :: Interactivity -> (OutputType -> Text -> IO ()) -> (ExitCode -> IO ()) -> P.CreateProcess -> IO ExitCode
run interactivity emit onExit process = mask $ \restore -> do
  (resources@(stdinHandle, stdoutHandle, stderrHandle, processHandle), processGroup) <- start
  mapM_ hClose stdinHandle
  -- Also reaps the process if this is stopped, so it's never cancelled.
  rootExit <- Async.asyncWithUnmask $ \unmask -> unmask $ P.waitForProcess processHandle
  outputForwarding <-
    Async.asyncWithUnmask $ \unmask ->
      unmask $
        Async.concurrently_
          (forwardOutput (emit Stdout) stdoutHandle)
          (forwardOutput (emit Stderr) stderrHandle)
  let stop = stopProcess processHandle rootExit processGroup
  exitCode <-
    restore
      ( do
          exitCode <- Async.wait rootExit
          onExit exitCode
          stop
          Async.wait outputForwarding
          return exitCode
      )
      -- The process has to stop before the output forwarding is cancelled: on
      -- Windows, reading the output can't be interrupted until its pipes close.
      `onException` ((stop `finally` Async.cancel outputForwarding) `finally` closeHandles resources)
  closeHandles resources
  return exitCode
  where
    configuredProcess = case interactivity of
      Isolated -> System.configureIsolatedProcess process {P.std_in = P.CreatePipe}
      Interactive ->
        process
          { P.std_in = P.Inherit,
            P.create_group = False,
            P.use_process_jobs = False,
            P.std_out = P.CreatePipe,
            P.std_err = P.CreatePipe
          }

    start = do
      resources@(_, _, _, processHandle) <- P.createProcess configuredProcess
      processGroup <-
        ( case interactivity of
            Isolated -> P.getPid processHandle
            Interactive -> return Nothing
        )
          `onException` emergencyCleanUp resources
      return (resources, processGroup)

    stopProcess processHandle rootExit processGroup = case interactivity of
      Isolated -> do
        stopped <- System.stopProcessGroup processHandle rootExit processGroup
        unless stopped $ throwIO ProcessGroupDidNotStop
      Interactive ->
        P.getProcessExitCode processHandle >>= \case
          Just _ -> return ()
          Nothing -> P.terminateProcess processHandle

forwardOutput :: (Text -> IO ()) -> Maybe Handle -> IO ()
forwardOutput _ Nothing = return ()
forwardOutput emit (Just handle) =
  runConduit $
    CB.sourceHandle handle .| CT.decodeUtf8Lenient .| CL.mapM_ emit

emergencyCleanUp :: (Maybe Handle, Maybe Handle, Maybe Handle, P.ProcessHandle) -> IO ()
emergencyCleanUp resources@(_, _, _, processHandle) =
  ( do
      P.terminateProcess processHandle
      void $ timeout System.hardStopTimeoutMicroseconds $ P.waitForProcess processHandle
  )
    `finally` closeHandles resources

closeHandles :: (Maybe Handle, Maybe Handle, Maybe Handle, P.ProcessHandle) -> IO ()
closeHandles (stdinHandle, stdoutHandle, stderrHandle, _) =
  mapM_ closeHandle [stdinHandle, stdoutHandle, stderrHandle]
  where
    closeHandle Nothing = return ()
    closeHandle (Just handle) = void (try (hClose handle) :: IO (Either IOException ()))
