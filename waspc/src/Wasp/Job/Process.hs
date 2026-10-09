{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}

module Wasp.Job.Process
  ( fromProc,
    ProcessDidNotStop (..),
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
import Data.Maybe (isJust)
import Data.Text (Text)
import System.Exit (ExitCode)
import System.IO (Handle, hClose)
import qualified System.Process as P
import System.Timeout (timeout)
import Wasp.Job.Common (Job (..), OutputType (..))
import Wasp.Job.CreateProcess (CreateJobProcess, asCreateProcess)
import Wasp.Util (secondsToMicroSeconds)
#if !mingw32_HOST_OS
import System.IO.Error (tryIOError)
import qualified System.Posix.Signals as Signals
#endif

data ProcessDidNotStop = ProcessDidNotStop
  deriving (Show, Eq)

instance Exception ProcessDidNotStop where
  displayException _ = "Could not stop a process that Wasp started. It may still be running."

-- | Runs the process to completion, emitting its stdout and stderr as the job's
-- output, and returns its exit code once all of it has been emitted.
--
-- If the job is stopped before the process finishes, this stops the process
-- and waits for it to exit. If it doesn't exit in time, this kills it, and if
-- it still doesn't exit, this throws 'ProcessDidNotStop'.
fromProc :: CreateJobProcess -> Job ExitCode
fromProc jobProcess = Job $ do
  process <- liftIO $ asCreateProcess jobProcess
  sink <- ask
  liftIO $ run (sink Nothing) process

run :: (OutputType -> Text -> IO ()) -> P.CreateProcess -> IO ExitCode
run emit process = mask $ \restore -> do
  resources@(stdinHandle, stdoutHandle, stderrHandle, processHandle) <-
    P.createProcess process {P.std_out = P.CreatePipe, P.std_err = P.CreatePipe}
  -- Also reaps the process if this is stopped, so it's never cancelled.
  processExit <- Async.asyncWithUnmask $ \unmask -> unmask $ P.waitForProcess processHandle
  outputForwarding <-
    Async.asyncWithUnmask $ \unmask ->
      unmask $
        Async.concurrently_
          (forwardOutput (emit Stdout) stdoutHandle)
          (forwardOutput (emit Stderr) stderrHandle)
  exitCode <-
    restore
      ( do
          mapM_ hClose stdinHandle
          exitCode <- Async.wait processExit
          Async.wait outputForwarding
          return exitCode
      )
      -- The process has to stop before the output forwarding is cancelled: on
      -- Windows, reading the output can't be interrupted until its pipes close.
      `onException` ( (stopProcess processHandle processExit `finally` Async.cancel outputForwarding)
                        `finally` closeHandles resources
                    )
  closeHandles resources
  return exitCode

-- | Asks the process to stop (with SIGTERM on *nix), and waits for it to exit.
-- If it doesn't exit in time, kills it, and if it still doesn't exit, throws
-- 'ProcessDidNotStop'.
--
-- We only signal the process itself, and rely on it to stop the processes it
-- started. We don't signal its process group: it's Wasp's own, so that would
-- also stop Wasp, its other jobs, and whatever started Wasp (e.g. an npm script
-- or a CI shell).
stopProcess :: P.ProcessHandle -> Async.Async ExitCode -> IO ()
stopProcess processHandle processExit = do
  P.terminateProcess processHandle
  stoppedInTime <- waitForExit gracefulStopTimeoutMicroseconds
  unless stoppedInTime $ do
    killProcess processHandle
    stoppedAfterKill <- waitForExit killTimeoutMicroseconds
    unless stoppedAfterKill $ throwIO ProcessDidNotStop
  where
    waitForExit timeoutMicroseconds = isJust <$> timeout timeoutMicroseconds (Async.waitCatch processExit)

-- | On Windows, 'P.terminateProcess' already kills the process.
killProcess :: P.ProcessHandle -> IO ()
#if mingw32_HOST_OS
killProcess = P.terminateProcess
#else
killProcess processHandle =
  -- The process might have exited in the meantime, so we ignore errors.
  P.getPid processHandle >>= mapM_ (tryIOError . Signals.signalProcess Signals.sigKILL)
#endif

-- | How long a process gets to exit by itself after being asked to stop, e.g.
-- to stop the processes it started, before it's killed.
gracefulStopTimeoutMicroseconds :: Int
gracefulStopTimeoutMicroseconds = secondsToMicroSeconds 2

killTimeoutMicroseconds :: Int
killTimeoutMicroseconds = secondsToMicroSeconds 2

forwardOutput :: (Text -> IO ()) -> Maybe Handle -> IO ()
forwardOutput _ Nothing = return ()
forwardOutput emit (Just handle) =
  runConduit $
    CB.sourceHandle handle .| CT.decodeUtf8Lenient .| CL.mapM_ emit

closeHandles :: (Maybe Handle, Maybe Handle, Maybe Handle, P.ProcessHandle) -> IO ()
closeHandles (stdinHandle, stdoutHandle, stderrHandle, _) =
  mapM_ closeHandle [stdinHandle, stdoutHandle, stderrHandle]
  where
    closeHandle Nothing = return ()
    closeHandle (Just handle) = void (try (hClose handle) :: IO (Either IOException ()))
