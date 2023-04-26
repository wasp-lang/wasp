module Wasp.Cli.Command.CreateNewProject.StarterTemplates.Templating where

import Control.Monad.IO.Class (liftIO)
import Data.List (foldl')
import qualified Data.Text as T
import StrongPath (Abs, Dir, Path', relfile, (</>))
import Wasp.Cli.Command.CreateNewProject.ProjectDescription (NewProjectAppName, NewProjectName)
import Wasp.Project (WaspProjectDir)
import qualified Wasp.SemanticVersion as SV
import qualified Wasp.Util.IO as IOUtil
import qualified Wasp.Version as WV

-- Template file for wasp file has placeholders in it that we want to replace
-- in the .wasp file we have written to the disk.
replaceTemplatePlaceholdersInWaspFile :: NewProjectAppName -> NewProjectName -> Path' Abs (Dir WaspProjectDir) -> IO ()
replaceTemplatePlaceholdersInWaspFile appName projectName projectDir = liftIO $ do
  mainWaspFileContent <- IOUtil.readFileStrict absMainWaspFile

  let replacedContent =
        foldl'
          (\acc (placeholder, value) -> T.replace (T.pack placeholder) (T.pack value) acc)
          mainWaspFileContent
          replacements

  IOUtil.writeFileFromText absMainWaspFile replacedContent
  where
    absMainWaspFile = projectDir </> [relfile|main.wasp|]
    replacements =
      [ ("__waspAppName__", show appName),
        ("__waspProjectName__", show projectName),
        ("__waspVersion__", waspVersionBounds)
      ]
    waspVersionBounds = show (SV.backwardsCompatibleWith WV.waspVersion)
