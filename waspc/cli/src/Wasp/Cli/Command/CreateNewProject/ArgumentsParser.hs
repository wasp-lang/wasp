module Wasp.Cli.Command.CreateNewProject.ArgumentsParser
  ( parseNewProjectArgs,
    NewProjectArgs (..),
  )
where

import Options.Applicative (defaultPrefs, execParserPure)
import qualified Options.Applicative as Opt
import Wasp.Cli.Command.Call (Arguments)

data NewProjectArgs = NewProjectArgs
  { _projectName :: Maybe String,
    _templateName :: Maybe String
  }

parseNewProjectArgs :: Arguments -> Either String NewProjectArgs
parseNewProjectArgs newArgs = parserResultToEither (execParserPure defaultPrefs newProjectArgsParserInfo newArgs)
  where
    newProjectArgsParserInfo :: Opt.ParserInfo NewProjectArgs
    newProjectArgsParserInfo = Opt.info (newProjectArgsParser Opt.<**> Opt.helper) Opt.fullDesc

    newProjectArgsParser :: Opt.Parser NewProjectArgs
    newProjectArgsParser =
      NewProjectArgs
        <$> Opt.optional projectNameParser
        <*> Opt.optional templateNameParser

    projectNameParser :: Opt.Parser String
    projectNameParser = Opt.strArgument $ Opt.metavar "PROJECT_NAME"

    templateNameParser :: Opt.Parser String
    templateNameParser =
      Opt.strOption $
        Opt.long "template"
          <> Opt.short 't'
          <> Opt.metavar "TEMPLATE_NAME"
          <> Opt.help "Template to use for the new project"

    parserResultToEither :: Opt.ParserResult NewProjectArgs -> Either String NewProjectArgs
    parserResultToEither (Opt.Success success) = Right success
    parserResultToEither (Opt.Failure failure) = Left $ show help
      where
        (help, _, _) = Opt.execFailure failure "wasp new"
    parserResultToEither (Opt.CompletionInvoked _) = error "Completion invoked when parsing 'wasp new', but this should never happen"
