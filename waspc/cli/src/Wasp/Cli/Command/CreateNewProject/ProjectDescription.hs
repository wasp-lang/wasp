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
import Data.Maybe (isNothing)
import Path.IO (doesDirExist)
import StrongPath (Abs, Dir, Path', fromAbsDir)
import StrongPath.Path (toPathAbsDir)
import System.Directory (doesDirectoryExist)
import Wasp.Analyzer.Parser (isValidWaspIdentifier)
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.CreateNewProject.ArgumentsParser
  ( NewProjectArgs (..),
    NewProjectTemplateArg (..),
  )
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (defaultStarterTemplate)
import Wasp.Cli.Command.CreateNewProject.Common (throwProjectCreationError)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates
  ( StarterTemplate (LocalStarterTemplate, localPath),
    findTemplateByString,
  )
import Wasp.Cli.FileSystem (getAbsPathToDirInCwd)
import qualified Wasp.Cli.Interactive as Interactive
import Wasp.Cli.Util.PathArgument (DirPathArgument)
import qualified Wasp.Cli.Util.PathArgument as PathArgument
import Wasp.Project.Common (WaspProjectDir)
import Wasp.Util (indent, kebabToCamelCase, whenM)

data NewProjectDescription = NewProjectDescription
  { _projectName :: NewProjectName,
    _appName :: NewProjectAppName,
    _template :: StarterTemplate,
    _absWaspProjectDir :: Path' Abs (Dir WaspProjectDir)
  }

newtype NewProjectName = NewProjectName String

instance Show NewProjectName where
  show (NewProjectName name) = name

newtype NewProjectAppName = NewProjectAppName String

instance Show NewProjectAppName where
  show (NewProjectAppName name) = name

{-
  There are two ways of getting the project description:

  1. From CLI arguments

     wasp new <project-name> [-t <template-name> | -c <template-dir>]

    - Project name is required.
    - Template name/dir is optional, if not provided, we use the default template.

  2. Interactively

     wasp new

    - Project name is required.
    - Template name is required, we ask the user to choose from available templates.
-}
obtainNewProjectDescription :: NewProjectArgs -> [StarterTemplate] -> Command NewProjectDescription
obtainNewProjectDescription NewProjectArgs {_projectName = projectNameArg, _templateArg = templateArg} starterTemplates = do
  projectName <- maybe askForName return projectNameArg
  appName <-
    either throwProjectCreationError pure $
      parseWaspProjectNameIntoAppName projectName

  let prefersInteractive = isNothing projectNameArg

  template <- case templateArg of
    Just (NamedTemplateArg templateName) -> findNamedTemplate starterTemplates templateName
    Just (CustomTemplateDirArg templatePath) -> findCustomTemplate templatePath
    Nothing ->
      if prefersInteractive
        then askForTemplate starterTemplates
        else return defaultStarterTemplate

  absWaspProjectDir <- obtainAvailableProjectDirPath projectName
  return $ mkNewProjectDescription projectName appName absWaspProjectDir template

askForName :: Command String
askForName =
  liftIO $ Interactive.askForRequiredInput "Enter the project name (e.g. my-project)"

askForTemplate :: [StarterTemplate] -> Command StarterTemplate
askForTemplate starterTemplates =
  liftIO $ Interactive.askToChoose "Choose a starter template" $ fromList starterTemplates

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

findNamedTemplate :: [StarterTemplate] -> String -> Command StarterTemplate
findNamedTemplate availableTemplates templateName =
  maybe invalidTemplateNameError return $
    findTemplateByString availableTemplates templateName
  where
    invalidTemplateNameError =
      throwProjectCreationError $
        "The template "
          <> show templateName
          <> " doesn't exist. Available starter templates are: "
          <> intercalate ", " (map show availableTemplates)
          <> "."

findCustomTemplate :: DirPathArgument -> Command StarterTemplate
findCustomTemplate templatePath = do
  absTemplatePath <- liftIO $ PathArgument.getDirPath templatePath
  templateExists <- liftIO $ doesDirectoryExist $ fromAbsDir absTemplatePath
  if templateExists
    then return $ LocalStarterTemplate {localPath = absTemplatePath}
    else makeInvalidCustomTemplatePathError absTemplatePath
  where
    makeInvalidCustomTemplatePathError absTemplatePath =
      throwProjectCreationError $
        "The directory "
          <> show (fromAbsDir absTemplatePath)
          <> " doesn't exist or can't be found."

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

mkNewProjectDescription :: String -> NewProjectAppName -> Path' Abs (Dir WaspProjectDir) -> StarterTemplate -> NewProjectDescription
mkNewProjectDescription projectName appName absWaspProjectDir template =
  NewProjectDescription
    { _projectName = NewProjectName projectName,
      _appName = appName,
      _template = template,
      _absWaspProjectDir = absWaspProjectDir
    }
