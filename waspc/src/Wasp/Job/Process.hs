{-# LANGUAGE ScopedTypeVariables #-}

module Wasp.Job.Process
  ( ProcessInput (..),
    runProcessAsJob,
    runInteractiveProcess,
    runNodeCommandAsJob,
    runNodeCommandAsJobWithExtraEnv,
    runInteractiveNodeCommandAsJobWithExtraEnv,
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
import qualified System.Info
import qualified System.Process as P
import UnliftIO.Exception (bracket, onException)
import qualified Wasp.Job as J
import qualified Wasp.Node.Version as NodeVersion

-- TODO:
--   Switch from Data.Conduit.Process to Data.Conduit.Process.Typed.
--   It is a new module meant to replace Data.Conduit.Process which is about to become deprecated.

data ProcessInput = CloseStdin | InheritStdin
  deriving (Eq)

-- | Runs a given process while streaming its stderr and stdout to provided channel.
--   Returns exit code of the process once it finishes, and also sends it to the channel.
--   Makes sure to terminate the process (or process group on *nix) if exception occurs.
runProcessAsJob :: ProcessInput -> P.CreateProcess -> J.JobType -> J.Job
runProcessAsJob input process jobType chan =
  bracket
    startProcess
    (\(_, _, sph) -> terminateStreamingProcess sph)
    runStreamingProcessAsJob
  where
    startProcess
      | input == CloseStdin && System.Info.os /= "mingw32" = do
          (CP.ClosedStream, stdoutStream, stderrStream, processHandle) <-
            CP.streamingProcess process {P.create_group = True}
          return (stdoutStream, stderrStream, processHandle)
      | otherwise = do
          (CP.Inherited, stdoutStream, stderrStream, processHandle) <- CP.streamingProcess process
          return (stdoutStream, stderrStream, processHandle)

    runStreamingProcessAsJob (stdoutStream, stderrStream, processHandle) = do
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

    terminateStreamingProcess = interruptProcess . CP.streamingProcessHandleRaw

runInteractiveProcess :: P.CreateProcess -> IO ExitCode
runInteractiveProcess process =
  P.withCreateProcess
    process {P.std_in = P.Inherit, P.std_out = P.Inherit, P.std_err = P.Inherit}
    (\_ _ _ processHandle -> P.waitForProcess processHandle `onException` interruptProcess processHandle)

interruptProcess :: P.ProcessHandle -> IO ()
interruptProcess processHandle =
  if System.Info.os == "mingw32"
    then P.terminateProcess processHandle
    else P.interruptProcessGroupOf processHandle

runNodeCommandAsJob :: Path' Abs (Dir a) -> String -> [String] -> J.JobType -> J.Job
runNodeCommandAsJob = runNodeCommandAsJobWithExtraEnv []

runNodeCommandAsJobWithExtraEnv :: [(String, String)] -> Path' Abs (Dir a) -> String -> [String] -> J.JobType -> J.Job
runNodeCommandAsJobWithExtraEnv = runNodeCommandAsJobWithInput CloseStdin

runInteractiveNodeCommandAsJobWithExtraEnv :: [(String, String)] -> Path' Abs (Dir a) -> String -> [String] -> J.JobType -> J.Job
runInteractiveNodeCommandAsJobWithExtraEnv = runNodeCommandAsJobWithInput InheritStdin

runNodeCommandAsJobWithInput :: ProcessInput -> [(String, String)] -> Path' Abs (Dir a) -> String -> [String] -> J.JobType -> J.Job
runNodeCommandAsJobWithInput input extraEnvVars fromDir command args jobType chan =
  NodeVersion.checkUserNodeAndNpmMeetWaspRequirements >>= \case
    NodeVersion.VersionCheckFail errorMsg -> exitWithError (ExitFailure 1) (T.pack errorMsg)
    NodeVersion.VersionCheckSuccess -> do
      envVars <- getAllEnvVars
      let nodeCommandProcess = (P.proc command args) {P.env = Just envVars, P.cwd = Just $ SP.fromAbsDir fromDir}
      runProcessAsJob input nodeCommandProcess jobType chan
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
