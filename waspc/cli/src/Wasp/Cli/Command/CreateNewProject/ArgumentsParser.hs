module Wasp.Cli.Command.CreateNewProject.ArgumentsParser
  ( parseNewProjectArgs,
    NewProjectArgs (..),
  )
where

import Options.Applicative (defaultPrefs, execParserPure)
import qualified Options.Applicative as Opt
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.CreateNewProject.Common (throwProjectCreationError)

data NewProjectArgs = NewProjectArgs
  { _projectName :: Maybe String,
    _templateName :: Maybe String
  }

parseNewProjectArgs :: Arguments -> Command NewProjectArgs
parseNewProjectArgs newArgs = parserResultToNewProjectArgs $ execParserPure defaultPrefs newProjectArgsParserInfo newArgs
  where
    newProjectArgsParserInfo :: Opt.ParserInfo NewProjectArgs
    newProjectArgsParserInfo = Opt.info (newProjectArgsParser Opt.<**> Opt.helper) Opt.fullDesc

    newProjectArgsParser :: Opt.Parser NewProjectArgs
    newProjectArgsParser = NewProjectArgs <$> Opt.optional projectNameParser <*> Opt.optional templateNameParser

    projectNameParser :: Opt.Parser String
    projectNameParser = Opt.strArgument $ Opt.metavar "PROJECT_NAME"

    templateNameParser :: Opt.Parser String
    templateNameParser =
      Opt.strOption $
        Opt.long "template"
          <> Opt.short 't'
          <> Opt.metavar "TEMPLATE_NAME"
          <> Opt.help "Template to use for the new project"

    parserResultToNewProjectArgs :: Opt.ParserResult NewProjectArgs -> Command NewProjectArgs
    parserResultToNewProjectArgs (Opt.Success success) = return success
    parserResultToNewProjectArgs (Opt.Failure failure) = throwProjectCreationError $ show help
      where
        (help, _, _) = Opt.execFailure failure "wasp new"
    parserResultToNewProjectArgs (Opt.CompletionInvoked _) = throwProjectCreationError "Completion invoked when parsing 'wasp new', but this should never happen"
