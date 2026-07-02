module Wasp.Cli.Command.CreateNewProject.ProjectDescription
  ( obtainNewProjectDescription,
    NewProjectDescription (..),
    NewProjectName (..),
    NewProjectAppName (..),
    parseWaspProjectNameIntoAppName,
    obtainAvailableTemplateOutputDirPath,
    getAbsWaspProjectDir,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import Data.List.NonEmpty (fromList)
import Data.Maybe (isNothing)
import Path.IO (doesDirExist)
import StrongPath (Abs, Dir, Path', (</>))
import StrongPath.Path (toPathAbsDir)
import Wasp.Analyzer.AST (isValidWaspIdentifier)
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.CreateNewProject.ArgumentsParser
  ( NewProjectArgs (..),
  )
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (defaultStarterTemplate)
import Wasp.Cli.Command.CreateNewProject.Common (TemplateOutputDir, throwProjectCreationError)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates
  ( StarterTemplate,
    findTemplateByString,
    waspProjectDirFromTemplateOutputDir,
  )
import Wasp.Cli.FileSystem (getAbsPathToDirInCwd)
import qualified Wasp.Cli.Interactive as Interactive
import Wasp.Project.Common (WaspProjectDir)
import Wasp.Util (indent, kebabToCamelCase, whenM)

data NewProjectDescription = NewProjectDescription
  { _projectName :: NewProjectName,
    _appName :: NewProjectAppName,
    _template :: StarterTemplate,
    _absTemplateOutputDir :: Path' Abs (Dir TemplateOutputDir)
  }

getAbsWaspProjectDir :: NewProjectDescription -> Path' Abs (Dir WaspProjectDir)
getAbsWaspProjectDir (NewProjectDescription {_absTemplateOutputDir = absTemplateOutputDir, _template = template}) =
  absTemplateOutputDir </> waspProjectDirFromTemplateOutputDir template

newtype NewProjectName = NewProjectName String

instance Show NewProjectName where
  show (NewProjectName name) = name

newtype NewProjectAppName = NewProjectAppName String

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
obtainNewProjectDescription :: NewProjectArgs -> [StarterTemplate] -> Command NewProjectDescription
obtainNewProjectDescription NewProjectArgs {_projectName = projectNameArg, _templateName = templateNameArg} starterTemplates = do
  projectName <- maybe askForName return projectNameArg
  appName <-
    either throwProjectCreationError pure $
      parseWaspProjectNameIntoAppName projectName

  let prefersInteractive = isNothing projectNameArg
      getFallbackTemplate =
        if prefersInteractive
          then askForTemplate starterTemplates
          else return defaultStarterTemplate

  template <- maybe getFallbackTemplate (findTemplateOrThrow starterTemplates) templateNameArg

  absTemplateOutputDir <- obtainAvailableTemplateOutputDirPath projectName
  return $ mkNewProjectDescription projectName appName absTemplateOutputDir template

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
        [ "The project's name is not in the valid format! The project's name:",
          indent 2 "- must start with a letter or an underscore",
          indent 2 "- must contain only letters, numbers, dashes, or underscores",
          indent 2 "- must not be a Wasp keyword"
        ]
  where
    appName = kebabToCamelCase projectName

findTemplateOrThrow :: [StarterTemplate] -> String -> Command StarterTemplate
findTemplateOrThrow availableTemplates templateName = case findTemplateByString availableTemplates templateName of
  Just template -> return template
  Nothing -> throwProjectCreationError invalidTemplateNameError
  where
    invalidTemplateNameError =
      "The template '"
        <> templateName
        <> "' doesn't exist. Available starter templates are: "
        <> intercalate ", " (map show availableTemplates)
        <> "."

obtainAvailableTemplateOutputDirPath :: String -> Command (Path' Abs (Dir TemplateOutputDir))
obtainAvailableTemplateOutputDirPath projectName = do
  absTemplateOutputDir <- getAbsPathToTemplateOutputDirInCwd projectName
  ensureTemplateOutputDirDoesNotExist projectName absTemplateOutputDir
  return absTemplateOutputDir
  where
    getAbsPathToTemplateOutputDirInCwd :: String -> Command (Path' Abs (Dir TemplateOutputDir))
    getAbsPathToTemplateOutputDirInCwd projectDirName = do
      liftIO (getAbsPathToDirInCwd projectDirName) >>= either throwError return
      where
        throwError err = throwProjectCreationError $ "Failed to get absolute path to template output dir: " ++ show err

    ensureTemplateOutputDirDoesNotExist :: String -> Path' Abs (Dir TemplateOutputDir) -> Command ()
    ensureTemplateOutputDirDoesNotExist projectDirName absTemplateOutputDir = do
      whenM (doesDirExist $ toPathAbsDir absTemplateOutputDir) $
        throwProjectCreationError $
          "Directory \"" ++ projectDirName ++ "\" is not empty."

mkNewProjectDescription :: String -> NewProjectAppName -> Path' Abs (Dir TemplateOutputDir) -> StarterTemplate -> NewProjectDescription
mkNewProjectDescription projectName appName absTemplateOutputDir template =
  NewProjectDescription
    { _projectName = NewProjectName projectName,
      _appName = appName,
      _template = template,
      _absTemplateOutputDir = absTemplateOutputDir
    }
