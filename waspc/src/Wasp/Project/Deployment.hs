module Wasp.Project.Deployment
  ( loadUserDockerfileContents,
    deploy,
  )
where

import Control.Monad.Extra (whenMaybeM)
import Data.Text (Text)
import qualified Data.Text.IO as T.IO
import StrongPath (Abs, Dir, Path', relfile, toFilePath, (</>))
import System.Directory (doesFileExist)
import Wasp.Project.Common (WaspProjectDir)
import Wasp.Project.WaspNpmPackage (runWaspNpmPackageScript, waspDeployScriptInWaspNpmPackageDir)
import Wasp.Util (checkExitSuccess)

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
  (>>= checkExitSuccess "Deploy command")
    <$> runWaspNpmPackageScript waspDir waspDeployScriptInWaspNpmPackageDir deployScriptArgs
  where
    deployScriptArgs = concat [cmdArgs, ["--wasp-exe", waspExe, "--wasp-project-dir", toFilePath waspDir]]
