module Wasp.Cli.Command.CreateNewProject.ProjectDescription
  ( obtainNewProjectDescription,
    NewProjectDescription (..),
    NewProjectName (..),
    NewProjectAppName (..),
    parseWaspProjectNameIntoAppName,
    obtainAvailableProjectDirPath,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import Data.List.NonEmpty (fromList)
import Path.IO (doesDirExist)
import StrongPath (Abs, Dir, Path')
import StrongPath.Path (toPathAbsDir)
import Wasp.Analyzer.Parser (isValidWaspIdentifier)
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.CreateNewProject.ArgumentsParser (NewProjectArgs (..))
import Wasp.Cli.Command.CreateNewProject.Common
  ( throwProjectCreationError,
  )
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.FeaturedStarterTemplates (defaultStarterTemplate)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.StarterTemplate (StarterTemplate)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.StarterTemplateId
  ( getStarterTemplateByIdOrThrow,
  )
import Wasp.Cli.FileSystem (getAbsPathToDirInCwd)
import qualified Wasp.Cli.Interactive as Interactive
import Wasp.Project (WaspProjectDir)
import Wasp.Util (indent, kebabToCamelCase, whenM)

data NewProjectDescription = NewProjectDescription
  { _projectName :: NewProjectName,
    _appName :: NewProjectAppName,
    _template :: StarterTemplate,
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
    - Template name is required, we ask the user to choose from featured templates.
-}
obtainNewProjectDescription :: NewProjectArgs -> [StarterTemplate] -> Command NewProjectDescription
obtainNewProjectDescription NewProjectArgs {_projectName = projectNameArg, _templateId = templateIdArg} starterTemplates =
  case projectNameArg of
    Just projectName -> obtainNewProjectDescriptionFromCliArgs projectName templateIdArg starterTemplates
    Nothing -> obtainNewProjectDescriptionInteractively templateIdArg starterTemplates

obtainNewProjectDescriptionFromCliArgs :: String -> Maybe String -> [StarterTemplate] -> Command NewProjectDescription
obtainNewProjectDescriptionFromCliArgs projectName templateIdArg featuredTemplates =
  obtainNewProjectDescriptionFromProjectNameAndTemplateArg
    projectName
    templateIdArg
    featuredTemplates
    (return defaultStarterTemplate)

obtainNewProjectDescriptionInteractively :: Maybe String -> [StarterTemplate] -> Command NewProjectDescription
obtainNewProjectDescriptionInteractively templateIdArg featuredTemplates = do
  projectName <- liftIO $ Interactive.askForRequiredInput "Enter the project name (e.g. my-project)"
  obtainNewProjectDescriptionFromProjectNameAndTemplateArg
    projectName
    templateIdArg
    featuredTemplates
    (liftIO askForTemplate)
  where
    askForTemplate = Interactive.askToChoose "Choose a starter template" $ fromList featuredTemplates

-- Common logic
obtainNewProjectDescriptionFromProjectNameAndTemplateArg ::
  String ->
  Maybe String ->
  [StarterTemplate] ->
  Command StarterTemplate ->
  Command NewProjectDescription
obtainNewProjectDescriptionFromProjectNameAndTemplateArg projectName templateIdArg featuredTemplates getStarterTemplateWhenNoArg = do
  absWaspProjectDir <- obtainAvailableProjectDirPath projectName
  selectedTemplate <- maybe getStarterTemplateWhenNoArg (getStarterTemplateByIdOrThrow featuredTemplates) templateIdArg
  mkNewProjectDescription projectName absWaspProjectDir selectedTemplate

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

mkNewProjectDescription :: String -> Path' Abs (Dir WaspProjectDir) -> StarterTemplate -> Command NewProjectDescription
mkNewProjectDescription projectName absWaspProjectDir template = do
  appName <- either throwProjectCreationError pure $ parseWaspProjectNameIntoAppName projectName
  return $
    NewProjectDescription
      { _projectName = NewProjectName projectName,
        _appName = appName,
        _template = template,
        _absWaspProjectDir = absWaspProjectDir
      }

parseWaspProjectNameIntoAppName :: String -> Either String NewProjectAppName
parseWaspProjectNameIntoAppName projectName
  | isValidWaspIdentifier appName = Right $ NewProjectAppName appName
  | otherwise =
      Left . intercalate "\n" $
        [ "The project's name is not in the valid format!",
          indent 2 "- It can start with a letter or an underscore.",
          indent 2 "- It can contain only letters, numbers, dashes, or underscores.",
          indent 2 "- It can't be a Wasp keyword."
        ]
  where
    appName = kebabToCamelCase projectName
