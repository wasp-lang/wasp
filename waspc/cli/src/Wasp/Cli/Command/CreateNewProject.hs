module Wasp.Cli.Command.CreateNewProject
  ( createNewProject,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import StrongPath (Abs, Dir, Path, System, relfile, (</>))
import System.Process (callCommand)
import UnliftIO.Exception (SomeException, try)
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.CreateNewProject.ArgumentsParser (parseNewProjectArgs)
import Wasp.Cli.Command.CreateNewProject.Common
  ( throwInvalidTemplateNameUsedError,
    throwProjectCreationError,
    waspVersionBounds,
  )
import Wasp.Cli.Command.CreateNewProject.FallbackTemplate (createProjectFromFallbackTemplate)
import Wasp.Cli.Command.CreateNewProject.ProjectDescription
  ( NewProjectDescription (..),
    createNewProjectDescription,
  )
import Wasp.Cli.Command.CreateNewProject.Templates (getStarterTemplateNames)
import Wasp.Cli.Command.Message (cliSendMessageC)
import qualified Wasp.Message as Msg
import Wasp.Project (WaspProjectDir)
import Wasp.Util (whenM)
import qualified Wasp.Util.IO as IOUtil
import qualified Wasp.Util.Terminal as Term

-- It receives all of the arguments that were passed to the `wasp new` command.
createNewProject :: Arguments -> Command ()
createNewProject argumentsList = do
  newProjectArgs <- parseNewProjectArgs argumentsList
  templateNamesFetchResult <- liftIO getStarterTemplateNames

  newProjectDescription <- createNewProjectDescription newProjectArgs templateNamesFetchResult

  createNewProjectFromNewProjectDescription newProjectDescription
  liftIO $ printGettingStartedInstructions $ _projectName newProjectDescription
  where
    printGettingStartedInstructions :: String -> IO ()
    printGettingStartedInstructions projectFolder = do
      putStrLn $ Term.applyStyles [Term.Green] ("Created new Wasp app in ./" ++ projectFolder ++ " directory!")
      putStrLn "To run it, do:"
      putStrLn ""
      putStrLn $ Term.applyStyles [Term.Bold] ("    cd " ++ projectFolder)
      putStrLn $ Term.applyStyles [Term.Bold] "    wasp start"

createNewProjectFromNewProjectDescription :: NewProjectDescription -> Command ()
createNewProjectFromNewProjectDescription
  NewProjectDescription
    { _projectName = projectName,
      _appName = appName,
      _templateName = maybeTemplateName,
      _absWaspProjectDir = absWaspProjectDir
    } = do
    case maybeTemplateName of
      Just templateName -> createProjectFromTemplate absWaspProjectDir projectName appName templateName
      Nothing -> liftIO $ createProjectFromFallbackTemplate absWaspProjectDir projectName appName

createProjectFromTemplate :: Path System Abs (Dir WaspProjectDir) -> String -> String -> String -> Command ()
createProjectFromTemplate absWaspProjectDir projectName appName templateName = do
  cliSendMessageC $ Msg.Start $ "Creating your project from the " ++ templateName ++ " template..."

  templatePath <- getPathToRemoteTemplate templateName

  let projectDir = projectName

  fetchTemplate templatePath projectDir
  ensureTemplateWasFetched

  replacePlaceholdersInWaspFile
  where
    getPathToRemoteTemplate :: String -> Command String
    getPathToRemoteTemplate templateFolderName = return $ waspTemplatesRepo ++ "/" ++ templateFolderName
      where
        -- gh: prefix means Github repo
        waspTemplatesRepo = "gh:wasp-lang/starters"

    fetchTemplate :: String -> String -> Command ()
    fetchTemplate templatePath projectDir = do
      liftIO (try executeCmd) >>= \case
        Left (e :: SomeException) -> throwProjectCreationError $ "Failed to create project from template: " ++ show e
        Right _ -> return ()
      where
        executeCmd = callCommand $ unwords command
        command = ["npx", "--yes", "giget@latest", templatePath, projectDir]

    -- giget doesn't fail if the template dir doesn't exist in the repo, so we need to check if the directory exists.
    ensureTemplateWasFetched :: Command ()
    ensureTemplateWasFetched =
      whenM
        (liftIO $ IOUtil.isDirectoryEmpty absWaspProjectDir)
        throwInvalidTemplateNameUsedError

    replacePlaceholdersInWaspFile :: Command ()
    replacePlaceholdersInWaspFile = liftIO $ do
      mainWaspFileContent <- IOUtil.readFileStrict absMainWaspFile

      let replacedContent =
            foldl
              (\acc (placeholder, value) -> T.replace (T.pack placeholder) (T.pack value) acc)
              mainWaspFileContent
              replacements

      IOUtil.writeFileFromText absMainWaspFile replacedContent
      where
        absMainWaspFile = absWaspProjectDir </> [relfile|main.wasp|]
        replacements =
          [ ("__waspAppName__", appName),
            ("__waspProjectName__", projectName),
            ("__waspVersion__", waspVersionBounds)
          ]
