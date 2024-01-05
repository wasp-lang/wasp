module Wasp.Cli.Command.CreateNewProject.StarterTemplates.GhRepo
  ( createProjectOnDiskFromGhRepoTemplate,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.CreateNewProject.Common (throwProjectCreationError)
import Wasp.Cli.Command.CreateNewProject.ProjectDescription (NewProjectAppName, NewProjectName)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.Templating (replaceTemplatePlaceholdersInWaspFile)
import Wasp.Cli.GithubRepo (GithubRepoRef, fetchFolderFromGithubRepoToDisk)
import Wasp.Project (WaspProjectDir)

createProjectOnDiskFromGhRepoTemplate ::
  Path' Abs (Dir WaspProjectDir) ->
  NewProjectName ->
  NewProjectAppName ->
  GithubRepoRef ->
  String ->
  Command ()
createProjectOnDiskFromGhRepoTemplate absWaspProjectDir projectName appName ghRepoRef templatePathInRepo = do
  fetchTheTemplateFromGhToDisk >>= either throwProjectCreationError pure
  -- NOTE: Placeholders in main.wasp files of templates were probably not the best idea,
  --   and we should likely drop the concept in the future, but we still do this replacement here
  --   in order to keep it all working for old templates. Once we stop using any templates that
  --   have placeholders in their main.wasp files, we can remove this line below.
  liftIO $ replaceTemplatePlaceholdersInWaspFile appName projectName absWaspProjectDir
  where
    fetchTheTemplateFromGhToDisk = do
      let templateFolderPath = fromJust . SP.parseRelDir $ templatePathInRepo
      liftIO $ fetchFolderFromGithubRepoToDisk ghRepoRef templateFolderPath absWaspProjectDir
