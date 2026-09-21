module Wasp.Job.Subprocess
  ( runChecked,
    runReturningExitCode,
  )
where

import Control.Concurrent.Async (Concurrently (..))
import Control.Monad.IO.Class (liftIO)
import Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Process as CP
import qualified Data.Conduit.Text as CT
import System.Exit (ExitCode)
import qualified System.Process as P
import UnliftIO.Exception (bracket, finally)
import Wasp.Job (JobAction, JobOutputKind (..), getJobOutputSink, requireExitSuccess)

-- | Runs the process to completion, failing the Job on a nonzero child exit.
runChecked :: P.CreateProcess -> JobAction ()
runChecked process = runReturningExitCode process >>= requireExitSuccess

-- TODO(#4575):
--   Switch from Data.Conduit.Process to Data.Conduit.Process.Typed.
--   It is a new module meant to replace Data.Conduit.Process which is about to become deprecated.

-- | Runs the process to completion and returns its exit status for explicit handling.
runReturningExitCode :: P.CreateProcess -> JobAction ExitCode
runReturningExitCode process = do
  emit <- getJobOutputSink
  liftIO $
    bracket
      (CP.streamingProcess process)
      cleanUpStreamingProcess
      (runStreamingProcessAndStreamOutput emit)
  where
    cleanUpStreamingProcess (_, _, _, streamingProcessHandle) =
      terminateStreamingProcess streamingProcessHandle
        `finally` CP.closeStreamingProcessHandle streamingProcessHandle

    runStreamingProcessAndStreamOutput emit (CP.Inherited, stdoutStream, stderrStream, processHandle) = do
      let forwardOutput outputKind stream =
            runConduit $
              stream .| CT.decodeUtf8Lenient .| CL.mapM_ (emit outputKind)

      runConcurrently $
        Concurrently (forwardOutput Stdout stdoutStream)
          *> Concurrently (forwardOutput Stderr stderrStream)
          *> Concurrently (CP.waitForStreamingProcess processHandle)

    terminateStreamingProcess streamingProcessHandle = do
      let processHandle = CP.streamingProcessHandleRaw streamingProcessHandle
      CP.getStreamingProcessExitCode streamingProcessHandle >>= \case
        Just _ -> return ()
        Nothing -> P.terminateProcess processHandle
