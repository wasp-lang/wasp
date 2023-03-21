module Wasp.Project.Deployment
  ( loadUserDockerfileContents,
    deploy,
  )
where

import Control.Concurrent (newChan)
import Control.Concurrent.Async (concurrently)
import Control.Monad.Except (void)
import Control.Monad.Extra (whenMaybeM)
import Data.Text (Text)
import qualified Data.Text.IO as T.IO
import StrongPath (Abs, Dir, Path', reldir, relfile, toFilePath, (</>))
import System.Directory (doesDirectoryExist, doesFileExist)
import qualified Wasp.Data as Data
import qualified Wasp.Generator.Job as J
import Wasp.Generator.Job.IO (printJobMsgsUntilExitReceived)
import Wasp.Generator.Job.Process (runNodeCommandAsJob)
import Wasp.Project.Common (WaspProjectDir)
import Wasp.Util (unlessM)

loadUserDockerfileContents :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe Text)
loadUserDockerfileContents waspDir = do
  let dockerfileAbsPath = toFilePath $ waspDir </> [relfile|Dockerfile|]
  whenMaybeM (doesFileExist dockerfileAbsPath) $ T.IO.readFile dockerfileAbsPath

-- | This will run our TS deploy project by passing all args from the Wasp CLI straight through.
-- The TS project is compiled to JS in CI and included in the data dir for the release archive.
-- If the project was not yet built locally (i.e. after they just installed a Wasp version), we do so.
deploy :: FilePath -> Path' Abs (Dir WaspProjectDir) -> [String] -> IO ()
deploy waspExe waspDir cmdArgs = do
  waspDataDir <- Data.getAbsDataDirPath
  let deployDir = waspDataDir </> [reldir|packages/deploy|]
  let nodeModulesDirExists = doesDirectoryExist . toFilePath $ deployDir </> [reldir|node_modules|]
  unlessM nodeModulesDirExists $
    runCommandAndPrintOutput $ runNodeCommandAsJob deployDir "npm" ["install"] J.Server
  let deployScriptArgs = ["dist/index.js"] ++ cmdArgs ++ ["--wasp-exe", waspExe, "--wasp-project-dir", toFilePath waspDir]
  -- NOTE: Here we are lying by saying we are running in the J.Server context.
  -- TODO: Consider adding a new context for these types of things, like J.Other or J.External.
  runCommandAndPrintOutput $ runNodeCommandAsJob deployDir "node" deployScriptArgs J.Server
  where
    runCommandAndPrintOutput job = do
      chan <- newChan
      void $ concurrently (printJobMsgsUntilExitReceived chan) (job chan)
