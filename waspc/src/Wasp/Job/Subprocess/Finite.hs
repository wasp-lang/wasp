module Wasp.Job.Subprocess.Finite
  ( run,
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
import Wasp.Job.Internal (JobAction, JobOutputKind (..), getJobOutputSink, writeJobOutput)

-- TODO(#4575):
--   Switch from Data.Conduit.Process to Data.Conduit.Process.Typed.
--   It is a new module meant to replace Data.Conduit.Process which is about to become deprecated.

run :: P.CreateProcess -> JobAction ExitCode
run process = do
  outputSink <- getJobOutputSink
  liftIO $
    bracket
      (CP.streamingProcess process)
      cleanUpStreamingProcess
      (runStreamingProcessAndStreamOutput outputSink)
  where
    cleanUpStreamingProcess (_, _, _, streamingProcessHandle) =
      terminateStreamingProcess streamingProcessHandle
        `finally` CP.closeStreamingProcessHandle streamingProcessHandle

    runStreamingProcessAndStreamOutput outputSink (CP.Inherited, stdoutStream, stderrStream, processHandle) = do
      let forwardOutput outputKind stream =
            runConduit $
              stream .| CT.decodeUtf8Lenient .| CL.mapM_ (writeJobOutput outputSink outputKind)

      runConcurrently $
        Concurrently (forwardOutput Stdout stdoutStream)
          *> Concurrently (forwardOutput Stderr stderrStream)
          *> Concurrently (CP.waitForStreamingProcess processHandle)

    -- This generic runner does not create a process group, so it owns only the
    -- root process. Group cleanup belongs to the managed implementation.
    terminateStreamingProcess streamingProcessHandle = do
      let processHandle = CP.streamingProcessHandleRaw streamingProcessHandle
      CP.getStreamingProcessExitCode streamingProcessHandle >>= \case
        Just _ -> return ()
        Nothing -> P.terminateProcess processHandle
