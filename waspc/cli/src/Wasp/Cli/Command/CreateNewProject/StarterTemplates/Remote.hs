module Wasp.Cli.Command.CreateNewProject.StarterTemplates.Remote
  ( createProjectOnDiskFromRemoteTemplate,
  )
where

import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.CreateNewProject.Common
  ( throwInvalidTemplateNameUsedError,
    throwProjectCreationError,
  )
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.Common (replaceTemplatePlaceholdersInWaspFile)
import Wasp.Project (WaspProjectDir)
import Wasp.Util (whenM)
import qualified Wasp.Util.IO as IOUtil
import Wasp.Util.NodeCommand (runNodeCommandWithoutOutput)

createProjectOnDiskFromRemoteTemplate :: Path' Abs (Dir WaspProjectDir) -> String -> String -> String -> Command ()
createProjectOnDiskFromRemoteTemplate absWaspProjectDir projectName appName templateName = do
  templateGithubURL <- getURLToGithubTemplate templateName
  fetchGithubTemplateToDisk absWaspProjectDir templateGithubURL
  liftIO $ replaceTemplatePlaceholdersInWaspFile appName projectName absWaspProjectDir
  where
    getURLToGithubTemplate :: String -> Command String
    getURLToGithubTemplate tmplName = return $ waspTemplatesRepo ++ "/" ++ templateFolderName
      where
        templateFolderName = tmplName
        -- gh: prefix means Github repo
        waspTemplatesRepo = "gh:wasp-lang/starters"

    fetchGithubTemplateToDisk :: Path' Abs (Dir WaspProjectDir) -> String -> Command ()
    fetchGithubTemplateToDisk projectDir templateGithubURL = do
      liftIO (runNodeCommandWithoutOutput command) >>= \case
        Left e -> throwProjectCreationError e
        Right _ -> ensureTemplateWasFetched
      where
        command = ["npx", "--yes", "giget@latest", templateGithubURL, SP.fromAbsDir projectDir]

        -- giget doesn't fail if the template dir doesn't exist in the repo, so we need to check if the directory exists.
        ensureTemplateWasFetched :: Command ()
        ensureTemplateWasFetched =
          whenM
            (liftIO $ IOUtil.isDirectoryEmpty projectDir)
            throwInvalidTemplateNameUsedError
