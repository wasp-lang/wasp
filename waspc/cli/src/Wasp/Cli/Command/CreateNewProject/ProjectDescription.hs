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
import Data.Char (isAlpha, isAlphaNum)
import Data.List (intercalate, nub)
import Data.List.NonEmpty (fromList)
import Data.Maybe (fromMaybe, maybeToList)
import Path.IO (doesDirExist)
import StrongPath (Abs, Dir, Path', (</>))
import StrongPath.Path (toPathAbsDir)
import System.Environment (lookupEnv)
import Wasp.AppSpec.Identifier (isValidWaspIdentifier)
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.CreateNewProject.ArgumentsParser
  ( NewProjectArgs (..),
    newProjectArgsParser,
  )
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (defaultStarterTemplate)
import Wasp.Cli.Command.CreateNewProject.Common (TemplateOutputDir, throwProjectCreationError)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates
  ( StarterTemplate,
    findTemplateByString,
    waspProjectDirFromTemplateOutputDir,
  )
import Wasp.Cli.FileSystem (getAbsPathToDirInCwd)
import Wasp.Cli.Util.Parser (requireInteractiveTerminal)
import Wasp.Project.Common (WaspProjectDir)
import Wasp.Project.PackageManager
  ( PackageManager,
    defaultPackageManager,
    getPackageManagerName,
    parsePackageManagerFromUserAgent,
    parsePackageManagerName,
    supportedPackageManagers,
  )
import Wasp.Util (indent, kebabToCamelCase, whenM)
import Wasp.Util.Interactive (InteractiveTerminal)
import qualified Wasp.Util.Interactive as Interactive

data NewProjectDescription = NewProjectDescription
  { _projectName :: NewProjectName,
    _appName :: NewProjectAppName,
    _template :: StarterTemplate,
    _packageManager :: PackageManager,
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

     wasp new <project-name> [-t <template-name>] [--package-manager <name>]

    - Project name is required.
    - Template name is optional, if not provided, we use the default template.
    - Package manager is optional, if not provided, we use the one that ran
      Wasp (e.g. pnpm for `pnpm dlx @wasp/cli new`), or the default one.
  2. Interactively

     wasp new

    - Project name is required.
    - Template name is required, we ask the user to choose from available templates.
    - Package manager is required, we ask the user to choose.
-}
obtainNewProjectDescription :: NewProjectArgs -> [StarterTemplate] -> Command NewProjectDescription
obtainNewProjectDescription NewProjectArgs {_projectName = projectNameArg, _templateName = templateNameArg, _packageManagerName = packageManagerNameArg} starterTemplates = do
  packageManagerThatRanWasp <- liftIO findPackageManagerThatRanWasp
  (projectName, appName, template, packageManager) <- case projectNameArg of
    Just projectName -> obtainArgumentsFromCliCall packageManagerThatRanWasp projectName
    Nothing -> obtainArgumentsInteractively packageManagerThatRanWasp

  absTemplateOutputDir <- obtainAvailableTemplateOutputDirPath projectName
  return $ mkNewProjectDescription projectName appName absTemplateOutputDir template packageManager
  where
    obtainArgumentsFromCliCall packageManagerThatRanWasp projectName = do
      appName <- either throwProjectCreationError pure $ parseWaspProjectNameIntoAppName projectName
      template <- maybe (return defaultStarterTemplate) (findTemplateOrThrow starterTemplates) templateNameArg
      packageManager <- maybe (return $ fromMaybe defaultPackageManager packageManagerThatRanWasp) findPackageManagerOrThrow packageManagerNameArg
      return (projectName, appName, template, packageManager)

    obtainArgumentsInteractively packageManagerThatRanWasp = do
      terminal <- requireInteractiveTerminal newProjectArgsParser
      projectName <- askForName terminal
      appName <- either throwProjectCreationError pure $ parseWaspProjectNameIntoAppName projectName
      template <- maybe (askForTemplate terminal starterTemplates) (findTemplateOrThrow starterTemplates) templateNameArg
      packageManager <- maybe (askForPackageManager terminal packageManagerThatRanWasp) findPackageManagerOrThrow packageManagerNameArg
      return (projectName, appName, template, packageManager)

askForName :: InteractiveTerminal -> Command String
askForName terminal =
  liftIO $ Interactive.askForRequiredInput terminal "Enter the project name (e.g. my-project)"

-- | Offers the package manager that ran Wasp first, which makes it the default.
askForPackageManager :: InteractiveTerminal -> Maybe PackageManager -> Command PackageManager
askForPackageManager terminal packageManagerThatRanWasp =
  liftIO $ parseChosenName <$> Interactive.askToChoose terminal "Choose a package manager" (fromList packageManagerNames)
  where
    packageManagerNames = getPackageManagerName <$> nub (maybeToList packageManagerThatRanWasp ++ supportedPackageManagers)
    parseChosenName = fromMaybe defaultPackageManager . parsePackageManagerName

findPackageManagerOrThrow :: String -> Command PackageManager
findPackageManagerOrThrow packageManagerName =
  maybe (throwProjectCreationError invalidPackageManagerError) return $ parsePackageManagerName packageManagerName
  where
    invalidPackageManagerError =
      "Wasp doesn't work with the package manager '"
        <> packageManagerName
        <> "'. Choose one of: "
        <> intercalate ", " (getPackageManagerName <$> supportedPackageManagers)
        <> "."

-- | E.g. pnpm for `pnpm dlx @wasp/cli new`. Nothing when the user ran a
-- globally installed Wasp.
findPackageManagerThatRanWasp :: IO (Maybe PackageManager)
findPackageManagerThatRanWasp = (>>= parsePackageManagerFromUserAgent) <$> lookupEnv "npm_config_user_agent"

askForTemplate :: InteractiveTerminal -> [StarterTemplate] -> Command StarterTemplate
askForTemplate terminal starterTemplates =
  liftIO $ Interactive.askToChoose terminal "Choose a starter template" $ fromList starterTemplates

parseWaspProjectNameIntoAppName :: String -> Either String NewProjectAppName
parseWaspProjectNameIntoAppName projectName
  | isValidProjectName projectName && isValidWaspIdentifier appName = Right $ NewProjectAppName appName
  | otherwise =
      Left . intercalate "\n" $
        [ "The project's name is not in a valid format! The project's name:",
          indent 2 "- must start with a letter or an underscore",
          indent 2 "- must contain only letters, numbers, dashes, or underscores",
          indent 2 "- must not be a Wasp keyword"
        ]
  where
    appName = kebabToCamelCase projectName

-- We validate the raw project name because it does not undergo the conversion
-- to app name, but the project directory is still created from it.
isValidProjectName :: String -> Bool
isValidProjectName [] = False
isValidProjectName (c : cs) = isStartChar c && all isNameChar cs
  where
    isStartChar ch = isAlpha ch || ch == '_'
    isNameChar ch = isAlphaNum ch || ch == '_' || ch == '-'

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
    ensureTemplateOutputDirDoesNotExist projectDirName absTemplateOutputDir =
      do
        whenM (doesDirExist $ toPathAbsDir absTemplateOutputDir)
        $ throwProjectCreationError
        $ "Directory `"
          ++ projectDirName
          ++ "` already exists. Choose a different name or delete the directory first."

mkNewProjectDescription :: String -> NewProjectAppName -> Path' Abs (Dir TemplateOutputDir) -> StarterTemplate -> PackageManager -> NewProjectDescription
mkNewProjectDescription projectName appName absTemplateOutputDir template packageManager =
  NewProjectDescription
    { _projectName = NewProjectName projectName,
      _appName = appName,
      _template = template,
      _packageManager = packageManager,
      _absTemplateOutputDir = absTemplateOutputDir
    }
