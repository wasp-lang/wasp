module Wasp.Cli.Command.CreateNewProject.StarterTemplates.Templating where

import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import StrongPath (Abs, Dir, File, Path', relfile, (</>))
import Wasp.Cli.Command.CreateNewProject.Common (defaultWaspVersionBounds)
import Wasp.Cli.Command.CreateNewProject.ProjectDescription (NewProjectAppName, NewProjectName)
import Wasp.Project.Common (WaspProjectDir)
import qualified Wasp.Util.IO as IOUtil

-- Template file for wasp file has placeholders in it that we want to replace
-- in the .wasp file we have written to the disk.
replaceTemplatePlaceholdersInWaspFile :: NewProjectAppName -> NewProjectName -> Path' Abs (Dir WaspProjectDir) -> IO ()
replaceTemplatePlaceholdersInWaspFile appName projectName projectDir =
  updateFileContent absMainWaspFile $ replacePlaceholders waspFileReplacements
  where
    updateFileContent :: Path' Abs (File f) -> (Text -> Text) -> IO ()
    updateFileContent absFilePath updateFn =
      IOUtil.readFileStrict absFilePath >>= IOUtil.writeFileFromText absFilePath . updateFn

    replacePlaceholders :: [(String, String)] -> Text -> Text
    replacePlaceholders replacements content = foldl' replacePlaceholder content replacements
      where
        replacePlaceholder content' (placeholder, value) = T.replace (T.pack placeholder) (T.pack value) content'

    absMainWaspFile = projectDir </> [relfile|main.wasp|]
    waspFileReplacements =
      [ ("__waspAppName__", show appName),
        ("__waspProjectName__", show projectName),
        ("__waspVersion__", defaultWaspVersionBounds)
      ]
