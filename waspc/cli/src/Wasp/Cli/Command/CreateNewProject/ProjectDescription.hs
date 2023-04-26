module Wasp.Cli.Command.CreateNewProject.ProjectDescription
  ( obtainNewProjectDescription,
    NewProjectDescription (..),
    NewProjectName (..),
    NewProjectAppName (..),
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Data.List (intercalate)
import Data.List.NonEmpty (fromList)
import Data.Maybe (isJust)
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
import Wasp.Util (indent, kebabToCamelCase)

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
obtainNewProjectDescription (NewProjectArgs projectNameArg templateNameArg) starterTemplateNames =
  case projectNameArg of
    Just projectName -> obtainNewProjectDescriptionFromCliArgs projectName templateNameArg starterTemplateNames
    Nothing -> obtainNewProjectDescriptionInteractively templateNameArg starterTemplateNames

obtainNewProjectDescriptionFromCliArgs :: String -> Maybe String -> [StarterTemplateName] -> Command NewProjectDescription
obtainNewProjectDescriptionFromCliArgs projectName templateNameArg availableTemplates = do
  absWaspProjectDir <- validateAndGetAbsProjectDir projectName

  selectedTemplate <- validateAndGetTemplateNameFromCliArgs
  mkNewProjectDescription projectName absWaspProjectDir selectedTemplate
  where
    validateAndGetTemplateNameFromCliArgs :: Command StarterTemplateName
    validateAndGetTemplateNameFromCliArgs = do
      let maybeTemplateName = templateNameArg >>= findTemplateNameByString availableTemplates
      throwIfUserProvidedInvalidTemplateNameViaArgs templateNameArg maybeTemplateName

      case maybeTemplateName of
        Just templateName -> return templateName
        Nothing -> return defaultStarterTemplateName

obtainNewProjectDescriptionInteractively :: Maybe String -> [StarterTemplateName] -> Command NewProjectDescription
obtainNewProjectDescriptionInteractively templateNameArg availableTemplates = do
  projectName <- liftIO $ Interactive.askForRequiredInput "Enter the project name (e.g. my-project)"
  absWaspProjectDir <- validateAndGetAbsProjectDir projectName

  selectedTemplate <- maybe (liftIO askForTemplateName) findTemplateNameByStringOrThrow templateNameArg
  mkNewProjectDescription projectName absWaspProjectDir selectedTemplate
  where
    findTemplateNameByStringOrThrow templateName = findTemplateNameByString availableTemplates templateName & maybe throwInvalidTemplateNameUsedError return
    askForTemplateName = Interactive.askToChoose "Choose a starter template" $ fromList availableTemplates

validateAndGetAbsProjectDir :: String -> Command (Path' Abs (Dir WaspProjectDir))
validateAndGetAbsProjectDir projectName = do
  absWaspProjectDir <- getAbsPathToNewProjectDirInCwd projectName
  ensureProjectDirDoesNotExist projectName absWaspProjectDir
  return absWaspProjectDir
  where
    getAbsPathToNewProjectDirInCwd :: String -> Command (Path' Abs (Dir WaspProjectDir))
    getAbsPathToNewProjectDirInCwd projectDir = do
      liftIO (getAbsPathToDirInCwd projectDir) >>= either throwError return
      where
        throwError err = throwProjectCreationError $ "Failed to get absolute path to Wasp project dir: " ++ show err
    ensureProjectDirDoesNotExist :: String -> Path' Abs (Dir WaspProjectDir) -> Command ()
    ensureProjectDirDoesNotExist projectDirName absWaspProjectDir = do
      dirExists <- doesDirExist $ toPathAbsDir absWaspProjectDir

      when dirExists $
        throwProjectCreationError $
          "Directory \"" ++ projectDirName ++ "\" is not empty."

throwIfUserProvidedInvalidTemplateNameViaArgs :: Maybe String -> Maybe StarterTemplateName -> Command ()
throwIfUserProvidedInvalidTemplateNameViaArgs templateNameArg maybeTemplateName = do
  let isTemplateNameArgProvided = isJust templateNameArg
      isTemplateNameFound = isJust maybeTemplateName
  when
    (isTemplateNameArgProvided && not isTemplateNameFound)
    throwInvalidTemplateNameUsedError

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
