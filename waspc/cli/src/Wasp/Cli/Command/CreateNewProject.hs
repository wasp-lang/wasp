module Wasp.Cli.Command.CreateNewProject
  ( createNewProject,
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Path.IO (copyDirRecur, doesDirExist)
import StrongPath (Abs, Dir, Path, Path', System, parseAbsDir, reldir, relfile, (</>))
import StrongPath.Path (toPathAbsDir)
import System.Directory (getCurrentDirectory)
import qualified System.FilePath as FP
import System.Process (callCommand)
import Text.Printf (printf)
import UnliftIO.Exception (SomeException, try)
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.CreateNewProject.Common (throwProjectCreationError)
import Wasp.Cli.Command.CreateNewProject.ProjectDescription (NewProjectDescription (..), initNewProjectDescription)
import Wasp.Cli.Command.Message (cliSendMessageC)
import qualified Wasp.Data as Data
import qualified Wasp.Message as Msg
import Wasp.Project (WaspProjectDir)
import qualified Wasp.SemanticVersion as SV
import Wasp.Util (whenM)
import qualified Wasp.Util.IO as IOUtil
import qualified Wasp.Util.Terminal as Term
import qualified Wasp.Version as WV

-- It receives all of the arguments that were passed to the `wasp new` command.
createNewProject :: Arguments -> Command ()
createNewProject newArgs = do
  newProjectDescription <- initNewProjectDescription newArgs
  createWaspProjectDir newProjectDescription
  liftIO $ printGettingStartedInstructions $ _projectName newProjectDescription
  where
    printGettingStartedInstructions :: String -> IO ()
    printGettingStartedInstructions projectFolder = do
      putStrLn $ Term.applyStyles [Term.Green] ("Created new Wasp app in ./" ++ projectFolder ++ " directory!")
      putStrLn "To run it, do:"
      putStrLn ""
      putStrLn $ Term.applyStyles [Term.Bold] ("    cd " ++ projectFolder)
      putStrLn $ Term.applyStyles [Term.Bold] "    wasp start"

createWaspProjectDir :: NewProjectDescription -> Command ()
createWaspProjectDir newProjectDescription@NewProjectDescription {_templateName = template} = do
  absWaspProjectDir <- getAbsoluteWaspProjectDir newProjectDescription
  dirExists <- doesDirExist $ toPathAbsDir absWaspProjectDir

  when dirExists $
    throwProjectCreationError $
      show absWaspProjectDir ++ " is an existing directory"

  createProjectFromProjectDescription absWaspProjectDir
  where
    createProjectFromProjectDescription absWaspProjectDir = do
      if isJust template
        then createProjectFromTemplate absWaspProjectDir newProjectDescription
        else liftIO $ do
          initializeProjectFromSkeleton absWaspProjectDir
          writeMainWaspFile absWaspProjectDir newProjectDescription

getAbsoluteWaspProjectDir :: NewProjectDescription -> Command (Path System Abs (Dir WaspProjectDir))
getAbsoluteWaspProjectDir (NewProjectDescription projectName _ _) = do
  absCwd <- liftIO getCurrentDirectory
  case parseAbsDir $ absCwd FP.</> projectName of
    Right sp -> return sp
    Left err ->
      throwProjectCreationError $
        "Failed to parse absolute path to wasp project dir: " ++ show err

-- Copies prepared files to the new project directory.
initializeProjectFromSkeleton :: Path' Abs (Dir WaspProjectDir) -> IO ()
initializeProjectFromSkeleton absWaspProjectDir = do
  dataDir <- Data.getAbsDataDirPath
  let absSkeletonDir = dataDir </> [reldir|Cli/templates/new|]
  copyDirRecur (toPathAbsDir absSkeletonDir) (toPathAbsDir absWaspProjectDir)

writeMainWaspFile :: Path System Abs (Dir WaspProjectDir) -> NewProjectDescription -> IO ()
writeMainWaspFile waspProjectDir (NewProjectDescription projectName appName _) = IOUtil.writeFile absMainWaspFile mainWaspFileContent
  where
    absMainWaspFile = waspProjectDir </> [relfile|main.wasp|]
    mainWaspFileContent =
      unlines
        [ "app %s {" `printf` appName,
          "  wasp: {",
          "    version: \"%s\"" `printf` waspVersionBounds,
          "  },",
          "  title: \"%s\"" `printf` projectName,
          "}",
          "",
          "route RootRoute { path: \"/\", to: MainPage }",
          "page MainPage {",
          "  component: import Main from \"@client/MainPage.jsx\"",
          "}"
        ]

createProjectFromTemplate :: Path System Abs (Dir WaspProjectDir) -> NewProjectDescription -> Command ()
createProjectFromTemplate absWaspProjectDir NewProjectDescription {_appName = appName, _projectName = projectName, _templateName = maybeTemplateName} = do
  templateName <- case maybeTemplateName of
    Just name -> return name
    Nothing -> throwProjectCreationError "Template name is not provided."

  cliSendMessageC $ Msg.Start $ "Creating your project from the " ++ templateName ++ " template..."

  templatePath <- getPathToRemoteTemplate templateName

  let projectDir = projectName

  fetchTemplate templatePath projectDir
  ensureTemplateWasFetched

  replacePlaceholdersInWaspFile
  where
    getPathToRemoteTemplate :: String -> Command String
    getPathToRemoteTemplate templateName = return $ waspTemplatesRepo ++ "/" ++ templateName
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
        command = ["npx", "giget@latest", templatePath, projectDir]

    -- gitget doesn't fail if the template dir doesn't exist in the repo, so we need to check if the directory exists.
    ensureTemplateWasFetched :: Command ()
    ensureTemplateWasFetched =
      whenM (liftIO $ IOUtil.isDirectoryEmpty absWaspProjectDir) $
        throwProjectCreationError "Are you sure that the template exists? 🤔 Check the list of templates here: https://github.com/wasp-lang/starters"

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

waspVersionBounds :: String
waspVersionBounds = show (SV.backwardsCompatibleWith WV.waspVersion)
