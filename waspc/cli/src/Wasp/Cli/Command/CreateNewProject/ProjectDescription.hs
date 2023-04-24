module Wasp.Cli.Command.CreateNewProject.ProjectDescription
  ( createNewProjectDescription,
    NewProjectDescription (..),
  )
where

import Control.Monad (when)
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
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.Common
  ( StarterTemplateName,
    findTemplateNameByString,
  )
import qualified Wasp.Cli.Interactive as Interactive
import Wasp.Project (WaspProjectDir)
import Wasp.Util (indent, kebabToCamelCase)

data NewProjectDescription = NewProjectDescription
  { _projectName :: String,
    _appName :: String,
    _templateName :: StarterTemplateName,
    _absWaspProjectDir :: Path' Abs (Dir WaspProjectDir)
  }

createNewProjectDescription :: NewProjectArgs -> [StarterTemplateName] -> Command NewProjectDescription
createNewProjectDescription (NewProjectArgs projectNameArg templateNameArg) availableTemplates =
  do
    projectName <- getOrAskProjectName projectNameArg
    absWaspProjectDir <- validateAndGetAbsProjectDir projectName

    selectedTemplate <- selectTemplate availableTemplates
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
      where
        ensureProjectDirDoesNotExist :: String -> Path' Abs (Dir WaspProjectDir) -> Command ()
        ensureProjectDirDoesNotExist projectDirName absWaspProjectDir = do
          dirExists <- doesDirExist $ toPathAbsDir absWaspProjectDir

          when dirExists $
            throwProjectCreationError $
              "Directory \"" ++ projectDirName ++ "\" is not empty."

    selectTemplate :: [StarterTemplateName] -> Command StarterTemplateName
    selectTemplate templateNames = do
      let maybeTemplateName = templateNameArg >>= findTemplateNameByString templateNames
      throwIfUserProvidedInvalidTemplateNameViaArgs maybeTemplateName
      getOrAskTemplateName maybeTemplateName
      where
        throwIfUserProvidedInvalidTemplateNameViaArgs :: Maybe StarterTemplateName -> Command ()
        throwIfUserProvidedInvalidTemplateNameViaArgs maybeTemplateName = do
          let isTemplateNameArgProvided = isJust templateNameArg
              isTemplateNameFound = isJust maybeTemplateName
          when
            (isTemplateNameArgProvided && not isTemplateNameFound)
            throwInvalidTemplateNameUsedError

        getOrAskTemplateName :: Maybe StarterTemplateName -> Command StarterTemplateName
        getOrAskTemplateName = \case
          Just templateName -> return templateName
          Nothing -> liftIO $ Interactive.askToChoose "Choose a starter template" templateNames

    mkNewProjectDescription :: String -> Path' Abs (Dir WaspProjectDir) -> StarterTemplateName -> Command NewProjectDescription
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
