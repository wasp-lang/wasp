module Wasp.Cli.Command.CreateNewProject.StarterTemplates.Remote
  ( createProjectOnDiskFromRemoteTemplate,
  )
where

import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Path')
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.CreateNewProject.ProjectDescription (NewProjectAppName, NewProjectName)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.Github (starterTemplateGithubRepo)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.Templating (replaceTemplatePlaceholdersInWaspFile)
import Wasp.Cli.GithubRepo (fetchFolderFromGithubRepoToDisk)
import Wasp.Project (WaspProjectDir)

createProjectOnDiskFromRemoteTemplate :: Path' Abs (Dir WaspProjectDir) -> NewProjectName -> NewProjectAppName -> String -> Command ()
createProjectOnDiskFromRemoteTemplate absWaspProjectDir projectName appName templateName = do
  liftIO $ fetchGithubTemplateToDisk absWaspProjectDir templateName
  liftIO $ replaceTemplatePlaceholdersInWaspFile appName projectName absWaspProjectDir
  where
    fetchGithubTemplateToDisk :: Path' Abs (Dir WaspProjectDir) -> String -> IO ()
    fetchGithubTemplateToDisk projectDir templateFolderName = fetchFolderFromGithubRepoToDisk starterTemplateGithubRepo templateFolderName projectDir

-- liftIO (runNodeCommandSilenced command) >>= \case
--   Left e -> throwProjectCreationError e
--   Right _ -> ensureTemplateWasFetched
-- where
--   command = ["npx", "--yes", "giget@latest", templateGithubURL, SP.fromAbsDir projectDir]

--   -- giget doesn't fail if the template dir doesn't exist in the repo, so we need to check if the directory exists.
--   ensureTemplateWasFetched :: Command ()
--   ensureTemplateWasFetched =
--     whenM
--       (liftIO $ IOUtil.isDirectoryEmpty projectDir)
--       throwInvalidTemplateNameUsedError

--   templateGithubURL = gigetSpecificTemplatesURL ++ "/" ++ templateFolderName

--   gigetSpecificTemplatesURL =
--     let (repoOwner, repoName) = starterTemplateGithubRepo
--      in -- gh: prefix means Github repo
--         printf "gh:%s/%s" repoOwner repoName
