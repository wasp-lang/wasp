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
createNewProjectDescription (NewProjectArgs projectNameArg templateNameArg forceFallbackTemplate) templateNamesFetchResult =
  do
    projectName <- getOrAskProjectName projectNameArg
    absWaspProjectDir <- validateAndGetAbsProjectDir projectName

    selectedTemplate <- selectTemplate templateNamesFetchResult forceFallbackTemplate
    mkNewProjectDescription projectName absWaspProjectDir selectedTemplate
  where
    getOrAskProjectName :: Maybe String -> Command String
    getOrAskProjectName = \case
      Just projectName -> return projectName
      Nothing -> liftIO $ Interactive.askForRequiredInput "Enter the project name (e.g. my-project)"

    validateAndGetAbsProjectDir :: String -> Command (Path' Abs (Dir WaspProjectDir))
    validateAndGetAbsProjectDir projectName = do
      absWaspProjectDir <- getAbsPathToNewProjectDirInCwd projectName
      ensureProjectDirDoesNotExist projectName absWaspProjectDir
      return absWaspProjectDir

    ensureProjectDirDoesNotExist :: String -> Path' Abs (Dir WaspProjectDir) -> Command ()
    ensureProjectDirDoesNotExist projectDirName absWaspProjectDir = do
      dirExists <- doesDirExist $ toPathAbsDir absWaspProjectDir

      when dirExists $
        throwProjectCreationError $
          "Directory \"" ++ projectDirName ++ "\" is not empty."

    selectTemplate :: StarterTemplateNamesFetchResult -> Bool -> Command NewProjectTemplate
    selectTemplate fetchResult forceFallback
      | forceFallback = return FallbackTemplate
      | otherwise = handleTemplateFetchResult
      where
        handleTemplateFetchResult = case fetchResult of
          Success templateNames -> useRemoteTemplate templateNames
          Failure -> useFallbackTemplateWithWarning

        useRemoteTemplate templateNames = do
          templateName <- getOrAskTemplateName templateNames templateNameArg
          ensureValidTemplateNameUsed templateName templateNames
          return $ RemoteTemplate templateName

        useFallbackTemplateWithWarning = do
          liftIO warnUserIfUsingUnavailableTemplate
          return FallbackTemplate

    getOrAskTemplateName :: StarterTemplateNames -> Maybe String -> Command String
    getOrAskTemplateName templateNames = \case
      Just templateName -> return templateName
      Nothing -> liftIO $ Interactive.askToChoose "Choose a starter template" $ templatesToList templateNames

    ensureValidTemplateNameUsed :: String -> StarterTemplateNames -> Command ()
    ensureValidTemplateNameUsed templateName templateNames = do
      unless
        (isOneOfAvailableTemplateNames templateName templateNames)
        throwInvalidTemplateNameUsedError

    warnUserIfUsingUnavailableTemplate :: IO ()
    warnUserIfUsingUnavailableTemplate = do
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
