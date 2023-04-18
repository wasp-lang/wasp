module Wasp.Cli.Command.CreateNewProject.ProjectDescription
  ( initNewProjectDescription,
    NewProjectDescription (..),
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import Wasp.Analyzer.Parser (isValidWaspIdentifier)
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.CreateNewProject.ArgumentsParser (NewProjectArgs (..), parseNewProjectArgs)
import Wasp.Cli.Command.CreateNewProject.Common (throwProjectCreationError)
import Wasp.Cli.Command.CreateNewProject.Templates (Templates, getStarterTemplates, templatesToList)
import qualified Wasp.Cli.Interactive as Interactive
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

createNewProjectDescription :: NewProjectArgs -> Maybe Templates -> Command NewProjectDescription
createNewProjectDescription (NewProjectArgs projectNameArg templateNameArg) maybeTemplates =
  case (projectNameArg, templateNameArg, maybeTemplates) of
    -- If both project name and template name are provided
    (Just projectName, Just templateName, _) -> mkNewProjectDescription projectName (Just templateName)
    -- Project name is provided, ask for template name
    (Just projectName, Nothing, Just templates) -> do
      templateName <- askTemplateName templates
      mkNewProjectDescription projectName templateName
    -- Ask for project name and template name
    (_, _, Just templates) -> do
      projectName <- askProjectName
      templateName <- askTemplateName templates
      mkNewProjectDescription projectName templateName
    -- Fallback: If there are not templates, and project name is provided
    (Just projectName, Nothing, Nothing) -> mkNewProjectDescription projectName Nothing
    -- Fallback: If there are no templates, but no project name is provided
    _anyOtherCase -> do
      projectName <- askProjectName
      mkNewProjectDescription projectName Nothing
  where
    askProjectName :: Command String
    askProjectName = liftIO $ Interactive.askForRequiredInput "Enter the project name (e.g. my-project)"

    askTemplateName :: Templates -> Command (Maybe String)
    askTemplateName templates = liftIO $ Interactive.askToChoose "Choose a starter template" $ templatesToList templates

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
