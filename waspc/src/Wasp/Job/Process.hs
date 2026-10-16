module Wasp.Job.Process
  ( fromProc,
  )
where

import qualified Control.Concurrent.Async as Async
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

-- | Runs the process to completion, emitting its stdout and stderr as the job's
-- output, and returns its exit code once all of it has been emitted.
--
-- If the job is stopped before the process finishes, this terminates the
-- process (SIGTERM on *nix) without waiting for it to exit. The same happens if
-- emitting the output fails. We only signal the process itself, and rely on it
-- to stop the processes it started.
fromProc :: CreateJobProcess -> Job ExitCode
fromProc jobProcess = Job $ do
  process <- liftIO $ asCreateProcess jobProcess
  sink <- ask
  liftIO $ run (sink Nothing) process

run :: (OutputType -> Text -> IO ()) -> P.CreateProcess -> IO ExitCode
run emit process =
  P.withCreateProcess process {P.std_out = P.CreatePipe, P.std_err = P.CreatePipe} $
    \stdinHandle stdoutHandle stderrHandle processHandle -> do
      mapM_ hClose stdinHandle
      fst
        <$> Async.concurrently
          (P.waitForProcess processHandle)
          ( Async.concurrently_
              (forwardOutput (emit Stdout) stdoutHandle)
              (forwardOutput (emit Stderr) stderrHandle)
          )

forwardOutput :: (Text -> IO ()) -> Maybe Handle -> IO ()
forwardOutput _ Nothing = return ()
forwardOutput emit (Just handle) =
  runConduit $
    CB.sourceHandle handle .| CT.decodeUtf8Lenient .| CL.mapM_ emit
