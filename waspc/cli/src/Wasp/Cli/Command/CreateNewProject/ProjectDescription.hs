module Wasp.Cli.Command.CreateNewProject.ProjectDescription
  ( obtainNewProjectDescription,
    NewProjectDescription (..),
    NewProjectName (..),
    NewProjectAppName (..),
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Data.List (intercalate)
import Data.List.NonEmpty (fromList)
import Path.IO (doesDirExist)
import StrongPath (Abs, Dir, Path')
import StrongPath.Path (toPathAbsDir)
import Wasp.Analyzer.Parser (isValidWaspIdentifier)
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.CreateNewProject.ArgumentsParser (NewProjectArgs (..))
import Wasp.Cli.Command.CreateNewProject.Common
  ( throwInvalidTemplateNameUsedError,
    throwProjectCreationError,
  )
import Wasp.Cli.Command.CreateNewProject.StarterTemplates (StarterTemplateName, defaultStarterTemplateName, findTemplateNameByString)
import Wasp.Cli.FileSystem (getAbsPathToDirInCwd)
import qualified Wasp.Cli.Interactive as Interactive
import Wasp.Project (WaspProjectDir)
import Wasp.Util (indent, kebabToCamelCase, whenM)

data NewProjectDescription = NewProjectDescription
  { _projectName :: NewProjectName,
    _appName :: NewProjectAppName,
    _templateName :: StarterTemplateName,
    _absWaspProjectDir :: Path' Abs (Dir WaspProjectDir)
  }

data NewProjectName = NewProjectName String

instance Show NewProjectName where
  show (NewProjectName name) = name

data NewProjectAppName = NewProjectAppName String

instance Show NewProjectAppName where
  show (NewProjectAppName name) = name

{-
  There are two ways of getting the project description:
  1. From CLI arguments

     wasp new <project-name> [-t <template-name>]

    - Project name is required.
    - Template name is optional, if not provided, we use the default template.
  2. Interactively

     wasp new

    - Project name is required.
    - Template name is required, we ask the user to choose from available templates.
-}
obtainNewProjectDescription :: NewProjectArgs -> [StarterTemplateName] -> Command NewProjectDescription
obtainNewProjectDescription NewProjectArgs {_projectName = projectNameArg, _templateName = templateNameArg} starterTemplateNames =
  case projectNameArg of
    Just projectName -> obtainNewProjectDescriptionFromCliArgs projectName templateNameArg starterTemplateNames
    Nothing -> obtainNewProjectDescriptionInteractively templateNameArg starterTemplateNames

obtainNewProjectDescriptionFromCliArgs :: String -> Maybe String -> [StarterTemplateName] -> Command NewProjectDescription
obtainNewProjectDescriptionFromCliArgs projectName templateNameArg availableTemplates =
  obtainNewProjectDescriptionFromProjectNameAndTemplateArg
    projectName
    templateNameArg
    availableTemplates
    (return defaultStarterTemplateName)

obtainNewProjectDescriptionInteractively :: Maybe String -> [StarterTemplateName] -> Command NewProjectDescription
obtainNewProjectDescriptionInteractively templateNameArg availableTemplates = do
  projectName <- liftIO $ Interactive.askForRequiredInput "Enter the project name (e.g. my-project)"
  obtainNewProjectDescriptionFromProjectNameAndTemplateArg
    projectName
    templateNameArg
    availableTemplates
    (liftIO askForTemplateName)
  where
    askForTemplateName = Interactive.askToChoose "Choose a starter template" $ fromList availableTemplates

-- Common logic
obtainNewProjectDescriptionFromProjectNameAndTemplateArg ::
  String ->
  Maybe String ->
  [StarterTemplateName] ->
  Command StarterTemplateName ->
  Command NewProjectDescription
obtainNewProjectDescriptionFromProjectNameAndTemplateArg projectName templateNameArg availableTemplates obtainTemplateWhenNoArg = do
  absWaspProjectDir <- obtainAvailableProjectDirPath projectName
  selectedTemplate <- maybe obtainTemplateWhenNoArg findTemplateNameOrThrow templateNameArg
  mkNewProjectDescription projectName absWaspProjectDir selectedTemplate
  where
    findTemplateNameOrThrow :: String -> Command StarterTemplateName
    findTemplateNameOrThrow templateName =
      findTemplateNameByString availableTemplates templateName
        & maybe throwInvalidTemplateNameUsedError return

obtainAvailableProjectDirPath :: String -> Command (Path' Abs (Dir WaspProjectDir))
obtainAvailableProjectDirPath projectName = do
  absWaspProjectDir <- getAbsPathToNewProjectDirInCwd projectName
  ensureProjectDirDoesNotExist projectName absWaspProjectDir
  return absWaspProjectDir
  where
    getAbsPathToNewProjectDirInCwd :: String -> Command (Path' Abs (Dir WaspProjectDir))
    getAbsPathToNewProjectDirInCwd projectDirName = do
      liftIO (getAbsPathToDirInCwd projectDirName) >>= either throwError return
      where
        throwError err = throwProjectCreationError $ "Failed to get absolute path to Wasp project dir: " ++ show err
    ensureProjectDirDoesNotExist :: String -> Path' Abs (Dir WaspProjectDir) -> Command ()
    ensureProjectDirDoesNotExist projectDirName absWaspProjectDir = do
      whenM (doesDirExist $ toPathAbsDir absWaspProjectDir) $
        throwProjectCreationError $
          "Directory \"" ++ projectDirName ++ "\" is not empty."

mkNewProjectDescription :: String -> Path' Abs (Dir WaspProjectDir) -> StarterTemplateName -> Command NewProjectDescription
mkNewProjectDescription projectName absWaspProjectDir templateName
  | isValidWaspIdentifier appName =
      return $
        NewProjectDescription
          { _projectName = NewProjectName projectName,
            _appName = NewProjectAppName appName,
            _templateName = templateName,
            _absWaspProjectDir = absWaspProjectDir
          }
  | otherwise =
      throwProjectCreationError $
        intercalate
          "\n"
          [ "The project's name is not in the valid format!",
            indent 2 "- It can start with a letter or an underscore.",
            indent 2 "- It can contain only letters, numbers, dashes, or underscores.",
            indent 2 "- It can't be a Wasp keyword."
          ]
  where
    appName = kebabToCamelCase projectName
