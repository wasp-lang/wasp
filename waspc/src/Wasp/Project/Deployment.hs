module Wasp.Project.Deployment
  ( loadUserDockerfileContents,
    deploy,
  )
where

import Control.Concurrent (newChan)
import Control.Concurrent.Async (concurrently)
import Control.Monad.Extra (whenMaybeM)
import Data.Text (Text)
import qualified Data.Text.IO as T.IO
import StrongPath (Abs, Dir, Path', relfile, toFilePath, (</>))
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import qualified Wasp.Generator.Job as J
import Wasp.Generator.Job.IO (printJobMsgsUntilExitReceived)
import Wasp.Package (Package (DeployPackage), runPackageAsJob)
import Wasp.Project.Common (WaspProjectDir)

loadUserDockerfileContents :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe Text)
loadUserDockerfileContents waspDir = do
  let dockerfileAbsPath = toFilePath $ waspDir </> [relfile|Dockerfile|]
  whenMaybeM (doesFileExist dockerfileAbsPath) $ T.IO.readFile dockerfileAbsPath

deploy ::
  -- | Path to wasp executable.
  FilePath ->
  Path' Abs (Dir WaspProjectDir) ->
  -- | All arguments from the Wasp CLI.
  [String] ->
  IO (Either String ())
deploy waspExe waspDir cmdArgs =
  let deployScriptArgs = concat [cmdArgs, ["--wasp-exe", waspExe, "--wasp-project-dir", toFilePath waspDir]]
   in runCommandAndPrintOutput $ runPackageAsJob DeployPackage deployScriptArgs
  where
    runCommandAndPrintOutput :: J.Job -> IO (Either String ())
    runCommandAndPrintOutput job = do
      chan <- newChan
      (_, exitCode) <- concurrently (printJobMsgsUntilExitReceived chan) (job chan)
      case exitCode of
        ExitSuccess -> return $ Right ()
        ExitFailure code -> return $ Left $ "Deploy command failed with exit code: " ++ show code
