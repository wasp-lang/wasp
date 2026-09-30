{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Wasp.Job.Process
  ( runProcessAsJob,
    runNodeCommandAsJob,
    runNodeCommandAsJobWithExtraEnv,
  )
where

import Control.Concurrent (writeChan)
import Control.Concurrent.Async (Concurrently (..))
import Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Process as CP
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
#if !defined(mingw32_HOST_OS)
import qualified System.Posix.Signals as Signals
#endif
import qualified System.Process as P
import UnliftIO.Exception (bracket)
import qualified Wasp.Job as J
import qualified Wasp.Node.Version as NodeVersion

-- TODO:
--   Switch from Data.Conduit.Process to Data.Conduit.Process.Typed.
--   It is a new module meant to replace Data.Conduit.Process which is about to become deprecated.

-- | Runs a given process while streaming its stderr and stdout to provided channel. Stdin is inherited.
--   Returns exit code of the process once it finishes, and also sends it to the channel.
--   Makes sure to stop the process if exception occurs.
runProcessAsJob :: P.CreateProcess -> J.JobType -> J.Job
runProcessAsJob process jobType chan =
  bracket
    (CP.streamingProcess process)
    (\(_, _, _, sph) -> terminateStreamingProcess sph)
    runStreamingProcessAsJob
  where
    runStreamingProcessAsJob (CP.Inherited, stdoutStream, stderrStream, processHandle) = do
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

      exitCode <-
        runConcurrently $
          Concurrently forwardStdoutToChan
            *> Concurrently forwardStderrToChan
            *> Concurrently (CP.waitForStreamingProcess processHandle)

      writeChan chan $
        J.JobMessage
          { J._data = J.JobExit exitCode,
            J._jobType = jobType
          }

      return exitCode

    terminateStreamingProcess streamingProcessHandle = do
      interruptProcess $ CP.streamingProcessHandleRaw streamingProcessHandle
      return $ ExitFailure 1

-- | On *nix, sends SIGINT (same as Ctrl+C) to the given process only, and relies on it to stop any
-- processes it started itself (e.g. npm forwards the signal to the script it runs, and nodemon
-- stops its whole process tree).
-- We don't signal the whole process group: the processes we start share it with Wasp, so that
-- would also interrupt Wasp itself, its other jobs, and whatever process started Wasp.
-- Windows has no signals, so there we terminate the process instead.
interruptProcess :: P.ProcessHandle -> IO ()
#if defined(mingw32_HOST_OS)
interruptProcess = P.terminateProcess
#else
interruptProcess processHandle =
  P.getPid processHandle >>= mapM_ (Signals.signalProcess Signals.sigINT)
#endif

runNodeCommandAsJob :: Path' Abs (Dir a) -> String -> [String] -> J.JobType -> J.Job
runNodeCommandAsJob = runNodeCommandAsJobWithExtraEnv []

runNodeCommandAsJobWithExtraEnv :: [(String, String)] -> Path' Abs (Dir a) -> String -> [String] -> J.JobType -> J.Job
runNodeCommandAsJobWithExtraEnv extraEnvVars fromDir command args jobType chan =
  NodeVersion.checkUserNodeAndNpmMeetWaspRequirements >>= \case
    NodeVersion.VersionCheckFail errorMsg -> exitWithError (ExitFailure 1) (T.pack errorMsg)
    NodeVersion.VersionCheckSuccess -> do
      envVars <- getAllEnvVars
      let nodeCommandProcess = (P.proc command args) {P.env = Just envVars, P.cwd = Just $ SP.fromAbsDir fromDir}
      runProcessAsJob nodeCommandProcess jobType chan
  where
    -- Haskell will use the first value for variable name it finds. Since env
    -- vars in 'extraEnvVars' should override the inherited env vars, we
    -- must prepend them.
    getAllEnvVars = (extraEnvVars ++) <$> getEnvironment
    exitWithError exitCode errorMsg = do
      writeChan chan $
        J.JobMessage
          { J._data = J.JobOutput errorMsg J.Stderr,
            J._jobType = jobType
          }
      writeChan chan $
        J.JobMessage
          { J._data = J.JobExit exitCode,
            J._jobType = jobType
          }
      return exitCode
