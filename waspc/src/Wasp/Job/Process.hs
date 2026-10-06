module Wasp.Job.Process
  ( ProcessGroupDidNotStop (..),
    command,
    interactive,
    run,
    run_,
  )
where

import Control.Concurrent.Async (Concurrently (..), withAsync)
import qualified Control.Concurrent.Async as Async
import Control.Exception (Exception (displayException), IOException, bracketOnError, finally, onException, throwIO, try)
import Control.Monad (unless, void)
import Control.Monad.IO.Class (liftIO)
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
-- sink before returning.
--
-- A process that doesn't inherit Wasp's stdin (see 'interactive') runs in its
-- own process group. Once its root process exits, or the job is cancelled, any
-- process left in the group is stopped. If the group doesn't stop in time,
-- this throws 'ProcessGroupDidNotStop'. A process whose stdin is
-- 'P.CreatePipe' gets an empty stdin.
run :: P.CreateProcess -> Job ExitCode
run process = do
  sink <- getSink
  liftIO $
    bracketOnError start cleanUp $ \(resources@(stdinHandle, stdoutHandle, stderrHandle, processHandle), processGroup) -> do
      mapM_ hClose stdinHandle
      exitCode <-
        withAsync (P.waitForProcess processHandle) $ \rootExit ->
          runConcurrently $
            Concurrently (forwardOutput sink Stdout stdoutHandle)
              *> Concurrently (forwardOutput sink Stderr stderrHandle)
              *> Concurrently (waitForRootAndStopGroup processHandle processGroup rootExit)
      closeHandles resources
      return exitCode
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

    start = do
      resources@(_, _, _, processHandle) <- P.createProcess configuredProcess
      processGroup <-
        (if isInteractive then return Nothing else P.getPid processHandle)
          `onException` emergencyCleanUp resources
      return (resources, processGroup)

    cleanUp (resources@(_, _, _, processHandle), processGroup) =
      ( if isInteractive
          then
            P.getProcessExitCode processHandle >>= \case
              Just _ -> return ()
              Nothing -> P.terminateProcess processHandle
          else withAsync (P.waitForProcess processHandle) $ \rootExit ->
            ensureGroupStopped processHandle rootExit processGroup
      )
        `finally` closeHandles resources

    ensureGroupStopped processHandle rootExit processGroup = do
      stopped <- System.stopProcessGroup processHandle rootExit processGroup
      unless stopped $ throwIO ProcessGroupDidNotStop

    waitForRootAndStopGroup processHandle processGroup rootExit = do
      exitCode <- Async.wait rootExit
      unless isInteractive $ ensureGroupStopped processHandle rootExit processGroup
      return exitCode

-- | Like 'run', but fails the job if the process exits with a nonzero code.
run_ :: P.CreateProcess -> Job ()
run_ process = run process >>= requireExitSuccess

forwardOutput :: Sink -> Stream -> Maybe Handle -> IO ()
forwardOutput _ _ Nothing = return ()
forwardOutput sink stream (Just handle) =
  runConduit $
    CB.sourceHandle handle .| CT.decodeUtf8Lenient .| CL.mapM_ (sink stream)

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
