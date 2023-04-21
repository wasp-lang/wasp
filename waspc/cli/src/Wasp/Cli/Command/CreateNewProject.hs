module Wasp.Cli.Command.CreateNewProject
  ( createNewProject,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.List (foldl')
import qualified Data.Text as T
import StrongPath (Abs, Dir, relfile, (</>))
import qualified StrongPath as SP
import StrongPath.Types (Path')
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
    NewProjectTemplate (..),
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

  createNewProjectOnTheDisk newProjectDescription
  liftIO $ printGettingStartedInstructions $ _projectName newProjectDescription
  where
    printGettingStartedInstructions :: String -> IO ()
    printGettingStartedInstructions projectFolder = do
      putStrLn $ Term.applyStyles [Term.Green] ("Created new Wasp app in ./" ++ projectFolder ++ " directory!")
      putStrLn "To run it, do:"
      putStrLn ""
      putStrLn $ Term.applyStyles [Term.Bold] ("    cd " ++ projectFolder)
      putStrLn $ Term.applyStyles [Term.Bold] "    wasp start"

createNewProjectOnTheDisk :: NewProjectDescription -> Command ()
createNewProjectOnTheDisk
  NewProjectDescription
    { _projectName = projectName,
      _appName = appName,
      _templateName = templateName,
      _absWaspProjectDir = absWaspProjectDir
    } = do
    case templateName of
      RemoteTemplate remoteTemplateName ->
        createProjectOnTheDiskFromRemoteTemplate absWaspProjectDir projectName appName remoteTemplateName
      FallbackTemplate ->
        liftIO $ createProjectFromFallbackTemplate absWaspProjectDir projectName appName

createProjectOnTheDiskFromRemoteTemplate :: Path' Abs (Dir WaspProjectDir) -> String -> String -> String -> Command ()
createProjectOnTheDiskFromRemoteTemplate absWaspProjectDir projectName appName templateName = do
  cliSendMessageC $ Msg.Start $ "Creating your project from the " ++ templateName ++ " template..."
  templatePath <- getPathToRemoteTemplate templateName
  fetchTemplateAndWriteToDisk absWaspProjectDir templatePath
  replaceTemplatePlaceholdersInWaspFile absWaspProjectDir
  where
    getPathToRemoteTemplate :: String -> Command String
    getPathToRemoteTemplate tmplName = return $ waspTemplatesRepo ++ "/" ++ templateFolderName
      where
        templateFolderName = tmplName
        -- gh: prefix means Github repo
        waspTemplatesRepo = "gh:wasp-lang/starters"

    fetchTemplateAndWriteToDisk :: Path' Abs (Dir WaspProjectDir) -> String -> Command ()
    fetchTemplateAndWriteToDisk projectDir templatePath = do
      liftIO (try executeCmd) >>= \case
        Left (e :: SomeException) -> throwProjectCreationError $ "Failed to create project from template: " ++ show e
        Right _ -> ensureTemplateWasFetched
      where
        -- TODO: Throw nice message if node is not installed.
        executeCmd = callCommand $ unwords command
        command = ["npx", "--yes", "giget@latest", templatePath, SP.fromAbsDir projectDir]

        -- giget doesn't fail if the template dir doesn't exist in the repo, so we need to check if the directory exists.
        ensureTemplateWasFetched :: Command ()
        ensureTemplateWasFetched =
          whenM
            (liftIO $ IOUtil.isDirectoryEmpty projectDir)
            throwInvalidTemplateNameUsedError

    -- Template file for wasp file has placeholders in it that we want to replace
    -- in the wasp file we have just written to the disk.
    replaceTemplatePlaceholdersInWaspFile :: Path' Abs (Dir WaspProjectDir) -> Command ()
    replaceTemplatePlaceholdersInWaspFile projectDir = liftIO $ do
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
          [ ("__waspAppName__", appName),
            ("__waspProjectName__", projectName),
            ("__waspVersion__", waspVersionBounds)
          ]
