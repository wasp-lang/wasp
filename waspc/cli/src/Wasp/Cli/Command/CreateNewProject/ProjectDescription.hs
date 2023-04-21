module Wasp.Cli.Command.CreateNewProject.ProjectDescription
  ( createNewProjectDescription,
    NewProjectDescription (..),
    NewProjectTemplate (..),
  )
where

import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import Data.Maybe (isJust)
import Path.IO (doesDirExist)
import StrongPath (Abs, Dir, Path')
import StrongPath.Path (toPathAbsDir)
import Wasp.Analyzer.Parser (isValidWaspIdentifier)
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.CreateNewProject.ArgumentsParser (NewProjectArgs (..))
import Wasp.Cli.Command.CreateNewProject.Common
  ( getAbsPathToNewProjectDirInCwd,
    throwInvalidTemplateNameUsedError,
    throwProjectCreationError,
  )
import Wasp.Cli.Command.CreateNewProject.Templates
  ( StarterTemplateNames,
    StarterTemplateNamesFetchResult (..),
    isOneOfAvailableTemplateNames,
    templatesToList,
  )
import Wasp.Cli.Common (waspWarns)
import qualified Wasp.Cli.Interactive as Interactive
import Wasp.Cli.Terminal (asWaspWarningMessage)
import Wasp.Project (WaspProjectDir)
import Wasp.Util (indent, kebabToCamelCase)

data NewProjectDescription = NewProjectDescription
  { _projectName :: String,
    _appName :: String,
    _templateName :: NewProjectTemplate,
    _absWaspProjectDir :: Path' Abs (Dir WaspProjectDir)
  }

data NewProjectTemplate = RemoteTemplate String | FallbackTemplate

createNewProjectDescription :: NewProjectArgs -> StarterTemplateNamesFetchResult -> Command NewProjectDescription
createNewProjectDescription (NewProjectArgs projectNameArg templateNameArg forceFallbackTemplate) templateNamesFetchResult = do
  projectName <- getOrAskProjectName projectNameArg

  let projectDir = projectName
  absWaspProjectDir <- getAbsPathToNewProjectDirInCwd projectDir
  ensureProjectDirDoesNotExist projectDir absWaspProjectDir

  case (templateNamesFetchResult, forceFallbackTemplate) of
    (_, True) -> do
      mkNewProjectDescription projectName absWaspProjectDir FallbackTemplate
    (Success templateNames, _) -> do
      templateName <- getOrAskTemplateName templateNames templateNameArg

      ensureValidTemplateNameUsed templateName templateNames

      mkNewProjectDescription projectName absWaspProjectDir $ RemoteTemplate templateName
    -- Sometimes due to network issues, we can't fetch the list of templates. In that case, we just
    -- create a project with the fallback template. If the user wanted to use some different template,
    -- we give him a warning that we are using the fallback template.
    (Failure, _) -> do
      liftIO warnUserIfTemplateNameProvided

      mkNewProjectDescription projectName absWaspProjectDir FallbackTemplate
  where
    getOrAskProjectName :: Maybe String -> Command String
    getOrAskProjectName = \case
      Just projectName -> return projectName
      Nothing -> liftIO $ Interactive.askForRequiredInput "Enter the project name (e.g. my-project)"

    getOrAskTemplateName :: StarterTemplateNames -> Maybe String -> Command String
    getOrAskTemplateName templateNames = \case
      Just templateName -> return templateName
      Nothing -> liftIO $ Interactive.askToChoose "Choose a starter template" $ templatesToList templateNames

    ensureProjectDirDoesNotExist :: String -> Path' Abs (Dir WaspProjectDir) -> Command ()
    ensureProjectDirDoesNotExist projectDirName absWaspProjectDir = do
      dirExists <- doesDirExist $ toPathAbsDir absWaspProjectDir

      when dirExists $
        throwProjectCreationError $
          "Directory \"" ++ projectDirName ++ "\" is not empty."

    ensureValidTemplateNameUsed :: String -> StarterTemplateNames -> Command ()
    ensureValidTemplateNameUsed templateName templateNames = do
      unless
        (isOneOfAvailableTemplateNames templateName templateNames)
        throwInvalidTemplateNameUsedError

    warnUserIfTemplateNameProvided :: IO ()
    warnUserIfTemplateNameProvided = do
      when (isJust templateNameArg) $
        waspWarns (asWaspWarningMessage "Could note download templates, using the fallback template.")

    mkNewProjectDescription :: String -> Path' Abs (Dir WaspProjectDir) -> NewProjectTemplate -> Command NewProjectDescription
    mkNewProjectDescription projectName absWaspProjectDir templateName
      | isValidWaspIdentifier appName =
          return $
            NewProjectDescription
              { _projectName = projectName,
                _appName = appName,
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
