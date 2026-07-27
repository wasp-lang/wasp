module Wasp.Job.Node
  ( makeCreateProcess,
    makeCreateProcessWithExtraEnv,
    run,
    runWithExtraEnv,
    makeJob,
    makeJobWithExtraEnv,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import qualified System.Process as P
import qualified Wasp.Job as Job
import qualified Wasp.Job.Subprocess as Subprocess
import qualified Wasp.Node.Version as NodeVersion

makeJob :: Path' Abs (Dir a) -> String -> [String] -> Job.JobKind -> Job.Job
makeJob = makeJobWithExtraEnv []

makeJobWithExtraEnv :: [(String, String)] -> Path' Abs (Dir a) -> String -> [String] -> Job.JobKind -> Job.Job
makeJobWithExtraEnv extraEnvVars fromDir command args jobKind =
  Job.makeJob jobKind $ do
    exitCode <- runWithExtraEnv extraEnvVars fromDir command args
    Job.requireExitSuccess exitCode

run :: Path' Abs (Dir a) -> String -> [String] -> Job.JobAction ExitCode
run = runWithExtraEnv []

runWithExtraEnv :: [(String, String)] -> Path' Abs (Dir a) -> String -> [String] -> Job.JobAction ExitCode
runWithExtraEnv extraEnvVars fromDir command args =
  liftIO NodeVersion.checkUserNodeAndNpmMeetWaspRequirements >>= \case
    NodeVersion.VersionCheckFail errorMsg -> do
      Job.emitJobOutput Job.Stderr $ T.pack errorMsg
      return $ ExitFailure 1
    NodeVersion.VersionCheckSuccess -> do
      nodeCommandProcess <- liftIO $ makeCreateProcessWithExtraEnv extraEnvVars fromDir command args
      Subprocess.run nodeCommandProcess

makeCreateProcess :: Path' Abs (Dir a) -> String -> [String] -> IO P.CreateProcess
makeCreateProcess = makeCreateProcessWithExtraEnv []

makeCreateProcessWithExtraEnv :: [(String, String)] -> Path' Abs (Dir a) -> String -> [String] -> IO P.CreateProcess
makeCreateProcessWithExtraEnv extraEnvVars fromDir command args = do
  envVars <- getAllEnvVars
  return $ (P.proc command args) {P.env = Just envVars, P.cwd = Just $ SP.fromAbsDir fromDir}
  where
    -- Haskell will use the first value for variable name it finds. Since env
    -- vars in 'extraEnvVars' should override the inherited env vars, we
    -- must prepend them.
    getAllEnvVars = (extraEnvVars ++) <$> getEnvironment
