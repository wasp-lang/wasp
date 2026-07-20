module Wasp.Job.Process
  ( runProcessAndStreamOutput,
    runProcessAsJob,
  )
where

import Control.Concurrent.Async (Concurrently (..))
import Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Process as CP
import qualified Data.Conduit.Text as CT
import qualified System.Process as P
import UnliftIO.Exception (bracket, finally)
import qualified Wasp.Job as J

-- TODO:
--   Switch from Data.Conduit.Process to Data.Conduit.Process.Typed.
--   It is a new module meant to replace Data.Conduit.Process which is about to become deprecated.

-- | Runs a top-level job and emits 'JobExit' when it finishes.
-- Internal child processes should use 'runProcessAndStreamOutput' instead.
runProcessAsJob :: P.CreateProcess -> J.JobType -> J.Job
runProcessAsJob process jobType = J.makeJob jobType $ runProcessAndStreamOutput process

-- | Runs a child process and streams its output without emitting 'JobExit'.
-- Use 'runProcessAsJob' for top-level jobs that should signal completion to job readers.
runProcessAndStreamOutput :: P.CreateProcess -> J.JobOutputSink -> IO ExitCode
runProcessAndStreamOutput process outputSink =
  bracket
    (CP.streamingProcess process)
    cleanUpStreamingProcess
    runStreamingProcessAndStreamOutput
  where
    cleanUpStreamingProcess (_, _, _, streamingProcessHandle) =
      terminateStreamingProcess streamingProcessHandle
        `finally` CP.closeStreamingProcessHandle streamingProcessHandle

    runStreamingProcessAndStreamOutput (CP.Inherited, stdoutStream, stderrStream, processHandle) = do
      let forwardOutput outputType stream =
            runConduit $
              stream .| CT.decodeUtf8Lenient .| CL.mapM_ (J.writeJobOutput outputSink outputType)

      runConcurrently $
        Concurrently (forwardOutput J.Stdout stdoutStream)
          *> Concurrently (forwardOutput J.Stderr stderrStream)
          *> Concurrently (CP.waitForStreamingProcess processHandle)

    -- This generic runner does not create a process group, so it owns only the
    -- root process. Group cleanup belongs to LongRunning, which creates one.
    terminateStreamingProcess :: CP.StreamingProcessHandle -> IO ()
    terminateStreamingProcess streamingProcessHandle = do
      let processHandle = CP.streamingProcessHandleRaw streamingProcessHandle
      CP.getStreamingProcessExitCode streamingProcessHandle >>= \case
        Just _ -> return ()
        Nothing -> P.terminateProcess processHandle
