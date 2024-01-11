module Wasp.Cli.Command.CreateNewProject.StarterTemplates.Templating where

import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import StrongPath (Abs, Dir, File, Path')
import Wasp.Cli.Command.CreateNewProject.Common (defaultWaspVersionBounds)
import Wasp.Cli.Command.CreateNewProject.ProjectDescription (NewProjectAppName, NewProjectName)
import Wasp.Project.Analyze (findWaspFile)
import Wasp.Project.Common (WaspProjectDir)
import qualified Wasp.Util.IO as IOUtil

-- | Template file for wasp file has placeholders in it that we want to replace
-- in the .wasp file we have written to the disk.
-- If no .wasp file was found in the project, do nothing.
replaceTemplatePlaceholdersInWaspFile ::
  NewProjectAppName -> NewProjectName -> Path' Abs (Dir WaspProjectDir) -> IO ()
replaceTemplatePlaceholdersInWaspFile appName projectName projectDir = do
  findWaspFile projectDir >>= \case
    Left _ -> return ()
    Right absMainWaspFile ->
      updateFileContentWith absMainWaspFile (replacePlaceholders waspFileReplacements)
  where
    updateFileContentWith :: Path' Abs (File f) -> (Text -> Text) -> IO ()
    updateFileContentWith absFilePath updateFn =
      IOUtil.readFileStrict absFilePath >>= IOUtil.writeFileFromText absFilePath . updateFn

    replacePlaceholders :: [(String, String)] -> Text -> Text
    replacePlaceholders replacements content = foldl' replacePlaceholder content replacements
      where
        replacePlaceholder content' (placeholder, value) = T.replace (T.pack placeholder) (T.pack value) content'

    waspFileReplacements =
      [ ("__waspAppName__", show appName),
        ("__waspProjectName__", show projectName)
      ]
