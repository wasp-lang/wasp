module Wasp.Cli.Command.CreateNewProject.Options
  ( getProjectInfo,
    NewProjectArgs (..),
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import Options.Applicative (defaultPrefs, execParserPure)
import qualified Options.Applicative as Opt
import Wasp.Analyzer.Parser (isValidWaspIdentifier)
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.CreateNewProject.Common (ProjectInfo (..), throwProjectCreationError)
import Wasp.Cli.Command.CreateNewProject.Http (getTemplatesJson)
import qualified Wasp.Cli.Question as Question
import Wasp.Util (indent, kebabToCamelCase)

data NewProjectArgs = NewProjectArgs
  { _optsProjectName :: Maybe String,
    _optsTemplateName :: Maybe String
  }

type Templates = [String]

getProjectInfo :: Arguments -> Command ProjectInfo
getProjectInfo newArgs = do
  newProjectArgs <- getNewProjectArgs newArgs
  templates <- liftIO getTemplatesJson
  createProjectInfoFromArgs newProjectArgs templates

createProjectInfoFromArgs :: NewProjectArgs -> Maybe Templates -> Command ProjectInfo
createProjectInfoFromArgs (NewProjectArgs projectNameArg templateNameArg) maybeTemplates =
  case (projectNameArg, templateNameArg, maybeTemplates) of
    -- Non-interactive mode
    (Just projectName, templateName, _) ->
      createProjectInfo projectName templateName
    -- Interactive mode
    (_, _, Just templates) -> do
      projectName <- askProjectName
      templateName <- askTemplateName templates
      createProjectInfo projectName templateName
    -- Interactive mode (no templates available)
    _anyOtherCase -> do
      projectName <- askProjectName
      createProjectInfo projectName Nothing
  where
    askProjectName :: Command String
    askProjectName = liftIO $ Question.queryNotNull "Enter the project name (e.g. my-project)"

    askTemplateName :: Templates -> Command (Maybe String)
    askTemplateName templates = do
      liftIO $ Question.queryWithOptions "Choose a template (optional)" templates

createProjectInfo :: String -> Maybe String -> Command ProjectInfo
createProjectInfo name templateName
  | isValidWaspIdentifier appName = return $ ProjectInfo {_projectName = name, _appName = appName, _templateName = templateName}
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
    appName = kebabToCamelCase name

getNewProjectArgs :: Arguments -> Command NewProjectArgs
getNewProjectArgs newArgs = extractArgs $ execParserPure defaultPrefs argsParserInfo newArgs

argsParserInfo :: Opt.ParserInfo NewProjectArgs
argsParserInfo = Opt.info (argsParser Opt.<**> Opt.helper) Opt.fullDesc

argsParser :: Opt.Parser NewProjectArgs
argsParser = NewProjectArgs <$> Opt.optional projectNameParser <*> Opt.optional templateNameParser

projectNameParser :: Opt.Parser String
projectNameParser = Opt.strArgument $ Opt.metavar "PROJECT_NAME"

templateNameParser :: Opt.Parser String
templateNameParser =
  Opt.strOption $
    Opt.long "template"
      <> Opt.short 't'
      <> Opt.metavar "TEMPLATE_NAME"
      <> Opt.help "Template to use for the new project"

extractArgs :: Opt.ParserResult NewProjectArgs -> Command NewProjectArgs
extractArgs (Opt.Success success) = return success
extractArgs (Opt.Failure failure) = throwProjectCreationError $ show help
  where
    (help, _, _) = Opt.execFailure failure "wasp new"
extractArgs (Opt.CompletionInvoked _) = throwProjectCreationError "Completion invoked when parsing 'wasp new', but this should never happen"
