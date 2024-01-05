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
import Wasp.Cli.GithubRepo (GithubRepoRef, fetchFolderFromGithubRepoToDisk)
import Wasp.Project (WaspProjectDir)

createProjectOnDiskFromGhRepoTemplate ::
  Path' Abs (Dir WaspProjectDir) ->
  GithubRepoRef ->
  String ->
  Command ()
createProjectOnDiskFromGhRepoTemplate absWaspProjectDir ghRepoRef templatePathInRepo = do
  fetchTheTemplateFromGhToDisk >>= either throwProjectCreationError pure
  where
    fetchTheTemplateFromGhToDisk = do
      let templateFolderPath = fromJust . SP.parseRelDir $ templatePathInRepo
      liftIO $ fetchFolderFromGithubRepoToDisk ghRepoRef templateFolderPath absWaspProjectDir
