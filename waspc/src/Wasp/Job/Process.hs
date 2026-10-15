{-# LANGUAGE CPP #-}

module Wasp.Job.Process
  ( fromProc,
  )
where

import qualified Control.Concurrent.Async as Async
import Control.Exception (IOException, bracket, finally, try)
import Control.Monad (void)
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
import Wasp.Job.Common (Job (..), OutputType (..))
import Wasp.Job.CreateProcess (CreateJobProcess, asCreateProcess)
#if !mingw32_HOST_OS
import System.IO.Error (tryIOError)
import qualified System.Posix.Signals as Signals
#endif

-- | Runs the process to completion, emitting its stdout and stderr as the job's
-- output, and returns its exit code once all of it has been emitted.
-- If the job is stopped before the process finishes, this interrupts the
-- process.
fromProc :: CreateJobProcess -> Job ExitCode
fromProc jobProcess = Job $ do
  process <- liftIO $ asCreateProcess jobProcess
  sink <- ask
  liftIO $ run (sink Nothing) process

run :: (OutputType -> Text -> IO ()) -> P.CreateProcess -> IO ExitCode
run emit process = bracket start stop waitForExit
  where
    start = do
      resources@(_, stdoutHandle, stderrHandle, processHandle) <-
        P.createProcess process {P.std_out = P.CreatePipe, P.std_err = P.CreatePipe}
      -- Also reaps the process if the job is stopped, so it's never cancelled.
      processExit <- Async.asyncWithUnmask $ \unmask -> unmask $ P.waitForProcess processHandle
      -- Runs outside of 'waitForExit', so that it's only cancelled once the
      -- process is interrupted: on Windows, reading the output can't be
      -- interrupted until its pipes close.
      outputForwarding <-
        Async.asyncWithUnmask $ \unmask ->
          unmask $
            Async.concurrently_
              (forwardOutput (emit Stdout) stdoutHandle)
              (forwardOutput (emit Stderr) stderrHandle)
      return (resources, processExit, outputForwarding)

    waitForExit ((stdinHandle, _, _, _), processExit, outputForwarding) = do
      mapM_ hClose stdinHandle
      exitCode <- Async.wait processExit
      Async.wait outputForwarding
      return exitCode

    -- If the process finished, it was already reaped, so there's nothing to
    -- interrupt, and its output forwarding already finished too.
    stop (resources@(_, _, _, processHandle), _, outputForwarding) =
      (interruptProcess processHandle `finally` Async.cancel outputForwarding)
        `finally` closeHandles resources

-- | Sends SIGINT to the process, like Ctrl+C does. We don't use SIGTERM
-- because some tools don't stop their child processes on it, e.g. nodemon
-- kills its child shell right away, and that leaves the shell's command
-- running.
--
-- We only signal the process itself, and rely on it to stop the processes it
-- started. We don't signal its process group: it's Wasp's own, so that would
-- also stop Wasp, its other jobs, and whatever started Wasp (e.g. an npm script
-- or a CI shell).
--
-- Windows has no signals, so there we terminate the process instead.
interruptProcess :: P.ProcessHandle -> IO ()
#if mingw32_HOST_OS
interruptProcess = P.terminateProcess
#else
interruptProcess processHandle =
  -- The process might have exited in the meantime, so we ignore errors.
  P.getPid processHandle >>= mapM_ (tryIOError . Signals.signalProcess Signals.sigINT)
#endif

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
