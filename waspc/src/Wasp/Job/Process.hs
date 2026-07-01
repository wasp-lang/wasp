module Wasp.Job.Process
  ( runProcessAndStreamOutput,
    runProcessAsJob,
  )
where

import Control.Concurrent (writeChan)
import Control.Concurrent.Async (Concurrently (..))
import Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Process as CP
import Data.Text.Encoding (decodeUtf8)
import qualified System.Info
import qualified System.Process as P
import UnliftIO.Exception (bracket)
import qualified Wasp.Job as J

-- TODO:
--   Switch from Data.Conduit.Process to Data.Conduit.Process.Typed.
--   It is a new module meant to replace Data.Conduit.Process which is about to become deprecated.

-- | Runs a child process and streams its output without emitting 'JobExit'.
-- Use 'runProcessAsJob' for top-level jobs that should signal completion to job readers.
runProcessAndStreamOutput :: P.CreateProcess -> J.JobType -> J.Job
runProcessAndStreamOutput = runProcessAndStreamOutputWithCleanup terminateStreamingProcess

runProcessAndStreamOutputWithCleanup :: (CP.StreamingProcessHandle -> IO ()) -> P.CreateProcess -> J.JobType -> J.Job
runProcessAndStreamOutputWithCleanup cleanup process jobType chan =
  bracket
    (CP.streamingProcess process)
    (\(_, _, _, sph) -> cleanup sph)
    runStreamingProcessAndStreamOutput
  where
    runStreamingProcessAndStreamOutput (CP.Inherited, stdoutStream, stderrStream, processHandle) = do
      let forwardStdoutToChan =
            runConduit $
              stdoutStream
                .| CL.mapM_
                  ( \bs ->
                      writeChan chan $
                        J.JobMessage
                          { J._data = J.JobOutput (decodeUtf8 bs) J.Stdout,
                            J._jobType = jobType
                          }
                  )

      let forwardStderrToChan =
            runConduit $
              stderrStream
                .| CL.mapM_
                  ( \bs ->
                      writeChan chan $
                        J.JobMessage
                          { J._data = J.JobOutput (decodeUtf8 bs) J.Stderr,
                            J._jobType = jobType
                          }
                  )

      runConcurrently $
        Concurrently forwardStdoutToChan
          *> Concurrently forwardStderrToChan
          *> Concurrently (CP.waitForStreamingProcess processHandle)

terminateStreamingProcess :: CP.StreamingProcessHandle -> IO ()
terminateStreamingProcess streamingProcessHandle = do
  let processHandle = CP.streamingProcessHandleRaw streamingProcessHandle
  -- Many commands we run spawn child processes, which can spawn their own children.
  -- On Unix, interrupting the process group preserves the existing cleanup policy
  -- better than terminating only the root process, even if the root process already
  -- exited. On Windows, interruptProcessGroupOf requires create_group=True, which
  -- this generic runner intentionally avoids because some top-level jobs inherit
  -- stdin. Wasp-owned long-running children should use Wasp.Job.Process.Managed instead.
  if System.Info.os == "mingw32"
    then
      P.getProcessExitCode processHandle >>= \case
        Just _ -> return ()
        Nothing -> P.terminateProcess processHandle
    else P.interruptProcessGroupOf processHandle

-- | Runs a top-level job and emits 'JobExit' when it finishes.
-- Internal child processes should use 'runProcessAndStreamOutput' instead.
runProcessAsJob :: P.CreateProcess -> J.JobType -> J.Job
runProcessAsJob process jobType chan = do
  exitCode <- runProcessAndStreamOutput process jobType chan

  writeChan chan $
    J.JobMessage
      { J._data = J.JobExit exitCode,
        J._jobType = jobType
      }

  return exitCode
