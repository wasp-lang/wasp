module Wasp.Job.Node
  ( makeNodeCommandProcessWithExtraEnv,
    runNodeCommandAndStreamOutputWithExtraEnv,
    runNodeCommandAsJob,
    runNodeCommandAsJobWithExtraEnv,
  )
where

import Control.Concurrent (writeChan)
import qualified Data.Text as T
import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import qualified System.Process as P
import qualified Wasp.Job as J
import Wasp.Job.Process (runProcessAndStreamOutput)
import qualified Wasp.Node.Version as NodeVersion

runNodeCommandAndStreamOutputWithExtraEnv :: [(String, String)] -> Path' Abs (Dir a) -> String -> [String] -> J.JobType -> J.Job
runNodeCommandAndStreamOutputWithExtraEnv extraEnvVars fromDir command args jobType chan =
  makeNodeCommandProcessWithExtraEnv extraEnvVars fromDir command args >>= \case
    Left errorMsg -> writeErrorOutput (ExitFailure 1) (T.pack errorMsg)
    Right nodeCommandProcess -> runProcessAndStreamOutput nodeCommandProcess jobType chan
  where
    writeErrorOutput exitCode errorMsg = do
      writeChan chan $
        J.JobMessage
          { J._data = J.JobOutput errorMsg J.Stderr,
            J._jobType = jobType
          }
      return exitCode

runNodeCommandAsJob :: Path' Abs (Dir a) -> String -> [String] -> J.JobType -> J.Job
runNodeCommandAsJob = runNodeCommandAsJobWithExtraEnv []

runNodeCommandAsJobWithExtraEnv :: [(String, String)] -> Path' Abs (Dir a) -> String -> [String] -> J.JobType -> J.Job
runNodeCommandAsJobWithExtraEnv extraEnvVars fromDir command args jobType chan = do
  exitCode <- runNodeCommandAndStreamOutputWithExtraEnv extraEnvVars fromDir command args jobType chan
  writeChan chan $
    J.JobMessage
      { J._data = J.JobExit exitCode,
        J._jobType = jobType
      }
  return exitCode

makeNodeCommandProcessWithExtraEnv :: [(String, String)] -> Path' Abs (Dir a) -> String -> [String] -> IO (Either String P.CreateProcess)
makeNodeCommandProcessWithExtraEnv extraEnvVars fromDir command args =
  NodeVersion.checkUserNodeAndNpmMeetWaspRequirements >>= \case
    NodeVersion.VersionCheckFail errorMsg -> return $ Left errorMsg
    NodeVersion.VersionCheckSuccess -> do
      envVars <- getAllEnvVars
      let nodeCommandProcess = (P.proc command args) {P.env = Just envVars, P.cwd = Just $ SP.fromAbsDir fromDir}
      return $ Right nodeCommandProcess
  where
    -- Haskell will use the first value for variable name it finds. Since env
    -- vars in 'extraEnvVars' should override the inherited env vars, we
    -- must prepend them.
    getAllEnvVars = (extraEnvVars ++) <$> getEnvironment
