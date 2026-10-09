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
import Wasp.Job.Common (Job (..), JobEnv (..), OutputType (..))
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
  env <- ask
  liftIO $ run (_sink env Nothing) (_processExitHook env) process

-- | Runs the process to completion, passing its output to the given function,
-- and returns its exit code once all of its output has been passed on. Calls
-- the given exit hook as soon as the process exits by itself.
run :: (OutputType -> Text -> IO ()) -> (ExitCode -> IO ()) -> P.CreateProcess -> IO ExitCode
run emit onExit process = mask $ \restore -> do
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
          onExit exitCode
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

-- | Interrupts the process, and waits for it to exit. If it doesn't exit in
-- time, kills it, and if it still doesn't exit, throws 'ProcessDidNotStop'.
--
-- We only signal the process itself, and rely on it to stop the processes it
-- started. We don't signal its process group: it's Wasp's own, so that would
-- also stop Wasp, its other jobs, and whatever started Wasp (e.g. an npm script
-- or a CI shell).
stopProcess :: P.ProcessHandle -> Async.Async ExitCode -> IO ()
stopProcess processHandle processExit = do
  interruptProcess processHandle
  stoppedInTime <- waitForExit gracefulStopTimeoutMicroseconds
  unless stoppedInTime $ do
    killProcess processHandle
    stoppedAfterKill <- waitForExit killTimeoutMicroseconds
    unless stoppedAfterKill $ throwIO ProcessDidNotStop
  where
    waitForExit timeoutMicroseconds = isJust <$> timeout timeoutMicroseconds (Async.waitCatch processExit)

-- | Sends SIGINT to the process, like Ctrl+C does. We don't use SIGTERM
-- because some tools don't stop their child processes on it, e.g. nodemon
-- kills its child shell right away, and that leaves the shell's command
-- running.
-- Windows has no signals, so there we terminate the process instead.
interruptProcess :: P.ProcessHandle -> IO ()
killProcess :: P.ProcessHandle -> IO ()

#if mingw32_HOST_OS
interruptProcess = P.terminateProcess

killProcess = P.terminateProcess
#else
interruptProcess = signalProcess Signals.sigINT

killProcess = signalProcess Signals.sigKILL

signalProcess :: Signals.Signal -> P.ProcessHandle -> IO ()
signalProcess signal processHandle =
  -- The process might have exited in the meantime, so we ignore errors.
  P.getPid processHandle >>= mapM_ (tryIOError . Signals.signalProcess signal)
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
