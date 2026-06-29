{-# LANGUAGE ScopedTypeVariables #-}

module Wasp.Job.Process
  ( runProcessAsJob,
    runNodeCommandAsJob,
    runNodeCommandAsJobWithExtraEnv,
    runManagedNodeCommandAsJob,
  )
where

import Control.Concurrent (writeChan)
import Control.Concurrent.Async (Concurrently (..))
import Control.Monad (void)
import Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Process as CP
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import qualified System.Process as P
import UnliftIO.Exception (bracket)
import qualified Wasp.Job as J
import qualified Wasp.Job.Process.Managed as Managed
import qualified Wasp.Node.Version as NodeVersion

-- TODO:
--   Switch from Data.Conduit.Process to Data.Conduit.Process.Typed.
--   It is a new module meant to replace Data.Conduit.Process which is about to become deprecated.

-- | Runs a given process while streaming its stderr and stdout to provided channel. Stdin is inherited.
--   Returns exit code of the process once it finishes, and also sends it to the channel.
--   Makes sure to terminate the process if exception occurs.
--   If Wasp must keep running after cancelling this job, use 'runManagedNodeCommandAsJob'.
runProcessAsJob :: P.CreateProcess -> J.JobType -> J.Job
runProcessAsJob = runProcessAsJobWithStop stopProcess

runProcessAsJobWithStop :: (P.ProcessHandle -> IO ()) -> P.CreateProcess -> J.JobType -> J.Job
runProcessAsJobWithStop stopProcessOnError process jobType chan =
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
      let processHandle = CP.streamingProcessHandleRaw streamingProcessHandle
      stopProcessOnError processHandle

stopProcess :: P.ProcessHandle -> IO ()
stopProcess processHandle = do
  processExitCode <- P.getProcessExitCode processHandle
  case processExitCode of
    Just _ -> return ()
    Nothing -> do
      P.terminateProcess processHandle
      void $ P.waitForProcess processHandle

runNodeCommandAsJob :: Path' Abs (Dir a) -> String -> [String] -> J.JobType -> J.Job
runNodeCommandAsJob = runNodeCommandAsJobWithExtraEnv []

runNodeCommandAsJobWithExtraEnv :: [(String, String)] -> Path' Abs (Dir a) -> String -> [String] -> J.JobType -> J.Job
runNodeCommandAsJobWithExtraEnv = runNodeCommandAsJobWithExtraEnvAndConfig id

-- | Runs a Node command whose lifecycle is owned by Wasp.
--   Use for long-running child processes that Wasp can stop while Wasp itself keeps running.
runManagedNodeCommandAsJob :: [(String, String)] -> Path' Abs (Dir a) -> String -> [String] -> J.JobType -> J.Job
runManagedNodeCommandAsJob = runNodeCommandAsJobWithExtraEnvAndConfigAndStop Managed.configureManagedCreateProcess Managed.stopManagedProcessTree

runNodeCommandAsJobWithExtraEnvAndConfig :: (P.CreateProcess -> P.CreateProcess) -> [(String, String)] -> Path' Abs (Dir a) -> String -> [String] -> J.JobType -> J.Job
runNodeCommandAsJobWithExtraEnvAndConfig configureProcess = runNodeCommandAsJobWithExtraEnvAndConfigAndStop configureProcess stopProcess

runNodeCommandAsJobWithExtraEnvAndConfigAndStop :: (P.CreateProcess -> P.CreateProcess) -> (P.ProcessHandle -> IO ()) -> [(String, String)] -> Path' Abs (Dir a) -> String -> [String] -> J.JobType -> J.Job
runNodeCommandAsJobWithExtraEnvAndConfigAndStop configureProcess stopProcessOnError extraEnvVars fromDir command args jobType chan =
  NodeVersion.checkUserNodeAndNpmMeetWaspRequirements >>= \case
    NodeVersion.VersionCheckFail errorMsg -> exitWithError (ExitFailure 1) (T.pack errorMsg)
    NodeVersion.VersionCheckSuccess -> do
      envVars <- getAllEnvVars
      let nodeCommandProcess = configureProcess (P.proc command args) {P.env = Just envVars, P.cwd = Just $ SP.fromAbsDir fromDir}
      runProcessAsJobWithStop stopProcessOnError nodeCommandProcess jobType chan
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
