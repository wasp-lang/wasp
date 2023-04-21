module Wasp.Cli.Command.CreateNewProject.ProjectDescription
  ( createNewProjectDescription,
    NewProjectDescription (..),
  )
where

import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import Data.Maybe (isJust)
import Path.IO (doesDirExist)
import StrongPath.Path (toPathAbsDir)
import Wasp.Analyzer.Parser (isValidWaspIdentifier)
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.CreateNewProject.ArgumentsParser (NewProjectArgs (..))
import Wasp.Cli.Command.CreateNewProject.Common (getAbsPathToNewProjectDirInCwd, throwInvalidTemplateNameUsedError, throwProjectCreationError)
import Wasp.Cli.Command.CreateNewProject.Templates
  ( StarterTemplateNames,
    StarterTemplateNamesFetchResult (..),
    isOneOfAvailableTemplateNames,
    templatesToList,
  )
import Wasp.Cli.Common (waspWarns)
import qualified Wasp.Cli.Interactive as Interactive
import Wasp.Cli.Terminal (asWaspWarningMessage)
import Wasp.Util (indent, kebabToCamelCase)

data NewProjectDescription = NewProjectDescription
  { _projectName :: String,
    _appName :: String,
    _templateName :: Maybe String
  }

createNewProjectDescription :: NewProjectArgs -> StarterTemplateNamesFetchResult -> Command NewProjectDescription
createNewProjectDescription (NewProjectArgs projectNameArg templateNameArg) templateNamesFetchResult = do
  projectName <- getOrAskProjectName projectNameArg

  ensureProjectDirDoesNotExist projectName

  case templateNamesFetchResult of
    Success templateNames -> do
      templateName <- getOrAskTemplateName templateNames templateNameArg

      ensureValidTemplateNameUsed templateName templateNames

      mkNewProjectDescription projectName $ Just templateName
    -- Sometimes due to network issues, we can't fetch the list of templates. In that case, we just
    -- create a project with the fallback template. If the user wanted to use some different template,
    -- we give him a warning that we are using the fallback template.
    Failure -> do
      liftIO warnUserIfTemplateNameProvided

      mkNewProjectDescription projectName Nothing
  where
    getOrAskProjectName :: Maybe String -> Command String
    getOrAskProjectName = \case
      Just projectName -> return projectName
      Nothing -> liftIO $ Interactive.askForRequiredInput "Enter the project name (e.g. my-project)"

    getOrAskTemplateName :: StarterTemplateNames -> Maybe String -> Command String
    getOrAskTemplateName templateNames = \case
      Just templateName -> return templateName
      Nothing -> liftIO $ Interactive.askToChoose "Choose a starter template" $ templatesToList templateNames

    ensureProjectDirDoesNotExist :: String -> Command ()
    ensureProjectDirDoesNotExist projectDir = do
      absWaspProjectDir <- getAbsPathToNewProjectDirInCwd projectDir
      dirExists <- doesDirExist $ toPathAbsDir absWaspProjectDir

      when dirExists $
        throwProjectCreationError $
          "Directory \"" ++ projectDir ++ "\" is not empty."

    ensureValidTemplateNameUsed :: String -> StarterTemplateNames -> Command ()
    ensureValidTemplateNameUsed templateName templateNames = do
      unless
        (isOneOfAvailableTemplateNames templateName templateNames)
        throwInvalidTemplateNameUsedError

    warnUserIfTemplateNameProvided :: IO ()
    warnUserIfTemplateNameProvided = do
      when (isJust templateNameArg) $
        waspWarns (asWaspWarningMessage "Could note download templates, using the fallback template.")

    mkNewProjectDescription :: String -> Maybe String -> Command NewProjectDescription
    mkNewProjectDescription projectName templateName
      | isValidWaspIdentifier appName = return $ NewProjectDescription {_projectName = projectName, _appName = appName, _templateName = templateName}
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
