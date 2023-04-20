module Wasp.Cli.Command.CreateNewProject.ProjectDescription
  ( initNewProjectDescription,
    NewProjectDescription (..),
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import Data.Maybe (isJust)
import Path.IO (doesDirExist)
import StrongPath.Path (toPathAbsDir)
import Wasp.Analyzer.Parser (isValidWaspIdentifier)
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.CreateNewProject.ArgumentsParser (NewProjectArgs (..), parseNewProjectArgs)
import Wasp.Cli.Command.CreateNewProject.Common (getAbsoluteWaspProjectDir, throwInvalidTemplateNameUsedError, throwProjectCreationError)
import Wasp.Cli.Command.CreateNewProject.Templates (StarterTemplateNames, getStarterTemplates, isValidTemplateName, templatesToList)
import Wasp.Cli.Common (waspWarns)
import qualified Wasp.Cli.Interactive as Interactive
import Wasp.Cli.Terminal (asWaspWarningMessage)
import Wasp.Util (indent, kebabToCamelCase)

data NewProjectDescription = NewProjectDescription
  { _projectName :: String,
    _appName :: String,
    _templateName :: Maybe String
  }

initNewProjectDescription :: Arguments -> Command NewProjectDescription
initNewProjectDescription newArgs = do
  newProjectArgs <- parseNewProjectArgs newArgs
  templates <- liftIO getStarterTemplates
  createNewProjectDescription newProjectArgs templates

createNewProjectDescription :: NewProjectArgs -> Maybe StarterTemplateNames -> Command NewProjectDescription
createNewProjectDescription (NewProjectArgs projectNameArg templateNameArg) maybeTemplateNames = do
  projectName <- case projectNameArg of
    Just projectName -> return projectName
    Nothing -> askProjectName

  let projectDir = projectName
  absWaspProjectDir <- getAbsoluteWaspProjectDir projectDir
  dirExists <- doesDirExist $ toPathAbsDir absWaspProjectDir

  when dirExists $
    throwProjectCreationError $
      "Directory \"" ++ projectDir ++ "\" is not empty."

  case maybeTemplateNames of
    Just templateNames -> do
      templateName <- case templateNameArg of
        Just templateName -> ensureValidTemplateName templateName templateNames
        Nothing -> askTemplateName templateNames

      mkNewProjectDescription projectName templateName
    -- Sometimes due to network issues, we can't fetch the list of templates. In that case, we just
    -- create a project with the bundled template (ignoring the template name argument)
    Nothing -> do
      -- If the user provided a template name, warn him that we can't fetch it
      liftIO $
        when (isJust templateNameArg) $
          waspWarns (asWaspWarningMessage "Could not download Wasp templates, using the default template.")
      mkNewProjectDescription projectName Nothing
  where
    askProjectName :: Command String
    askProjectName = liftIO $ Interactive.askForRequiredInput "Enter the project name (e.g. my-project)"

    askTemplateName :: StarterTemplateNames -> Command (Maybe String)
    askTemplateName templateNames = liftIO $ Interactive.askToChoose "Choose a starter template" $ templatesToList templateNames

    ensureValidTemplateName :: String -> StarterTemplateNames -> Command (Maybe String)
    ensureValidTemplateName templateName templateNames =
      if isValidTemplateName templateName templateNames
        then return $ Just templateName
        else throwInvalidTemplateNameUsedError

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
