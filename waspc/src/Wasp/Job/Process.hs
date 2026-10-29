module Wasp.Job.Process
  ( fromProc,
  )
where

import qualified Control.Concurrent.Async as Async
import Control.Exception (onException)
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
import System.Timeout (timeout)
import Wasp.Job.Common (Job (..), OutputType (..))
import Wasp.Job.CreateProcess (CreateJobProcess, asCreateProcess)
import Wasp.Util (secondsToMicroSeconds)

-- | Runs the process to completion, emitting its stdout and stderr as the job's
-- output, and returns its exit code once all of it has been emitted.
fromProc :: CreateJobProcess -> Job ExitCode
fromProc jobProcess = Job $ do
  process <- liftIO $ asCreateProcess jobProcess
  sink <- ask
  liftIO $ run (sink Nothing) process

-- | Runs the process to completion, passing its output to the given function,
-- and returns its exit code once all of its output has been passed on.
--
-- If this is stopped before the process finishes, it terminates the process
-- (SIGTERM on *nix) without waiting for it to exit. The same happens if
-- emitting the output fails. We only signal the process itself, and rely on it
-- to stop the processes it started.
run :: (OutputType -> Text -> IO ()) -> P.CreateProcess -> IO ExitCode
run emit process =
  P.withCreateProcess process {P.std_out = P.CreatePipe, P.std_err = P.CreatePipe} $
    \stdinHandle stdoutHandle stderrHandle processHandle ->
      -- Forwarded outside of `concurrently`, so that it keeps going if the job
      -- is stopped, and the output can be drained.
      Async.withAsync
        ( Async.concurrently_
            (forwardOutput (emit Stdout) stdoutHandle)
            (forwardOutput (emit Stderr) stderrHandle)
        )
        $ \outputForwarding -> do
          mapM_ hClose stdinHandle
          (fst <$> Async.concurrently (P.waitForProcess processHandle) (Async.wait outputForwarding))
            `onException` drainOutputIfProcessExited processHandle outputForwarding
  where
    -- If the process exited by itself, the rest of its output, e.g. a crash's
    -- stack trace, is about to be emitted.
    drainOutputIfProcessExited processHandle outputForwarding =
      P.getProcessExitCode processHandle >>= \case
        Just _ -> void $ timeout outputDrainTimeoutMicroseconds $ Async.waitCatch outputForwarding
        Nothing -> return ()

-- | How long a job that is stopped after its process exited still emits the
-- process's output for. It's bounded because the process's children can keep
-- its output open.
outputDrainTimeoutMicroseconds :: Int
outputDrainTimeoutMicroseconds = secondsToMicroSeconds 1

forwardOutput :: (Text -> IO ()) -> Maybe Handle -> IO ()
forwardOutput _ Nothing = return ()
forwardOutput emit (Just handle) =
  runConduit $
    CB.sourceHandle handle .| CT.decodeUtf8Lenient .| CL.mapM_ emit
