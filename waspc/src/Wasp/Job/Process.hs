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
import System.Exit (ExitCode (..))
import qualified System.Info
import qualified System.Process as P
import UnliftIO.Exception (bracket)
import qualified Wasp.Job as J

-- TODO:
--   Switch from Data.Conduit.Process to Data.Conduit.Process.Typed.
--   It is a new module meant to replace Data.Conduit.Process which is about to become deprecated.

-- | Runs a top-level job and emits 'JobExit' when it finishes.
-- Internal child processes should use 'runProcessAndStreamOutput' instead.
runProcessAsJob :: P.CreateProcess -> J.JobType -> J.Job
runProcessAsJob process jobType = J.makeJob jobType $ runProcessAndStreamOutput process jobType

-- | Runs a child process and streams its output without emitting 'JobExit'.
-- Use 'runProcessAsJob' for top-level jobs that should signal completion to job readers.
runProcessAndStreamOutput :: P.CreateProcess -> J.JobType -> J.JobOutputStreamer
runProcessAndStreamOutput process jobType chan =
  bracket
    (CP.streamingProcess process)
    (\(_, _, _, sph) -> terminateStreamingProcess sph)
    runStreamingProcessAndStreamOutput
  where
    runStreamingProcessAndStreamOutput (CP.Inherited, stdoutStream, stderrStream, processHandle) = do
      let forwardStdoutToChan =
            runConduit $
              stdoutStream .| CT.decodeUtf8Lenient .| CL.mapM_ (\text -> J.writeJobOutput jobType J.Stdout text chan)

      let forwardStderrToChan =
            runConduit $
              stderrStream .| CT.decodeUtf8Lenient .| CL.mapM_ (\text -> J.writeJobOutput jobType J.Stderr text chan)

      runConcurrently $
        Concurrently forwardStdoutToChan
          *> Concurrently forwardStderrToChan
          *> Concurrently (CP.waitForStreamingProcess processHandle)

    -- NOTE(shayne): On *nix, we use interruptProcessGroupOf instead of terminateProcess because many
    -- processes we run will spawn child processes, which themselves may spawn child processes.
    -- We want to ensure the entire process chain is stopped.
    -- We are limiting support of this to *nix only now, as Windows requires create_group=True
    -- but that surfaces an issue where a new process group that needs stdin but is started as a
    -- background process gets terminated, appearing to hang.
    -- Ref: https://stackoverflow.com/questions/61856063/spawning-a-process-with-create-group-true-set-pgid-hangs-when-starting-docke
    terminateStreamingProcess streamingProcessHandle = do
      let processHandle = CP.streamingProcessHandleRaw streamingProcessHandle
      if System.Info.os == "mingw32"
        then P.terminateProcess processHandle
        else P.interruptProcessGroupOf processHandle
      return $ ExitFailure 1
