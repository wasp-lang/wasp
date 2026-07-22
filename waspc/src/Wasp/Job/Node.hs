module Wasp.Job.Node
  ( makeNodeCommandProcessWithExtraEnv,
    runNodeCommandAndStreamOutputWithExtraEnv,
    runNodeCommandAsJob,
    runNodeCommandAsJobWithExtraEnv,
  )
where

import qualified Data.Text as T
import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import qualified System.Process as P
import qualified Wasp.Job as J
import Wasp.Job.Process (runProcessAndStreamOutput)
import qualified Wasp.Node.Version as NodeVersion

runNodeCommandAsJob :: Path' Abs (Dir a) -> String -> [String] -> J.JobType -> J.Job
runNodeCommandAsJob = runNodeCommandAsJobWithExtraEnv []

runNodeCommandAsJobWithExtraEnv :: [(String, String)] -> Path' Abs (Dir a) -> String -> [String] -> J.JobType -> J.Job
runNodeCommandAsJobWithExtraEnv extraEnvVars fromDir command args jobType =
  J.makeJob jobType $
    runNodeCommandAndStreamOutputWithExtraEnv extraEnvVars fromDir command args

runNodeCommandAndStreamOutputWithExtraEnv :: [(String, String)] -> Path' Abs (Dir a) -> String -> [String] -> J.JobOutputSink -> IO ExitCode
runNodeCommandAndStreamOutputWithExtraEnv extraEnvVars fromDir command args outputSink =
  NodeVersion.checkUserNodeAndNpmMeetWaspRequirements >>= \case
    NodeVersion.VersionCheckFail errorMsg -> do
      J.writeJobOutput outputSink J.Stderr $ T.pack errorMsg
      return $ ExitFailure 1
    NodeVersion.VersionCheckSuccess -> do
      nodeCommandProcess <- makeNodeCommandProcessWithExtraEnv extraEnvVars fromDir command args
      runProcessAndStreamOutput nodeCommandProcess outputSink

makeNodeCommandProcessWithExtraEnv :: [(String, String)] -> Path' Abs (Dir a) -> String -> [String] -> IO P.CreateProcess
makeNodeCommandProcessWithExtraEnv extraEnvVars fromDir command args = do
  envVars <- getAllEnvVars
  return $ (P.proc command args) {P.env = Just envVars, P.cwd = Just $ SP.fromAbsDir fromDir}
  where
    -- Haskell will use the first value for variable name it finds. Since env
    -- vars in 'extraEnvVars' should override the inherited env vars, we
    -- must prepend them.
    getAllEnvVars = (extraEnvVars ++) <$> getEnvironment
