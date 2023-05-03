{-# LANGUAGE ScopedTypeVariables #-}

module Wasp.Generator.Job.Process
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
import qualified System.Info
import qualified System.Process as P
import UnliftIO.Exception (bracket)
import qualified Wasp.Generator.Job as J
import qualified Wasp.Generator.Node.Version as NodeVersion
import qualified Wasp.SemanticVersion as SV

-- TODO:
--   Switch from Data.Conduit.Process to Data.Conduit.Process.Typed.
--   It is a new module meant to replace Data.Conduit.Process which is about to become deprecated.

-- | Runs a given process while streaming its stderr and stdout to provided channel. Stdin is inherited.
--   Returns exit code of the process once it finishes, and also sends it to the channel.
--   Makes sure to terminate the process (or process group on *nix) if exception occurs.
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

runNodeCommandAsJob :: Path' Abs (Dir a) -> String -> [String] -> J.JobType -> J.Job
runNodeCommandAsJob = runNodeCommandAsJobWithExtraEnv []

runNodeCommandAsJobWithExtraEnv :: [(String, String)] -> Path' Abs (Dir a) -> String -> [String] -> J.JobType -> J.Job
runNodeCommandAsJobWithExtraEnv extraEnvVars fromDir command args jobType chan =
  NodeVersion.getNodeVersion >>= \case
    Left errorMsg -> exitWithError (ExitFailure 1) (T.pack errorMsg)
    Right nodeVersion ->
      if SV.isVersionInRange nodeVersion NodeVersion.nodeVersionRange
        then do
          envVars <- getAllEnvVars
          let nodeCommandProcess = (P.proc command args) {P.env = Just envVars, P.cwd = Just $ SP.fromAbsDir fromDir}
          runProcessAsJob nodeCommandProcess jobType chan
        else exitWithError (ExitFailure 1) (T.pack $ NodeVersion.makeNodeVersionMismatchMessage nodeVersion)
  where
    -- Haskell will use the first value for variable name it finds. Since env
    -- vars in 'extraEnvVars' should override the the inherited env vars, we
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
