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
    _templateName :: Maybe String,
    -- Useful for e2e tests so we don't rely on remote templates
    _forceFallbackTemplate :: Bool
  }

parseNewProjectArgs :: Arguments -> Command NewProjectArgs
parseNewProjectArgs newArgs = do
  let newProjectArgsOrError =
        parserResultToNewProjectArgs $
          execParserPure defaultPrefs newProjectArgsParserInfo newArgs
  case newProjectArgsOrError of
    Right newProjectArgs -> return newProjectArgs
    Left err -> throwProjectCreationError err
  where
    newProjectArgsParserInfo :: Opt.ParserInfo NewProjectArgs
    newProjectArgsParserInfo = Opt.info (newProjectArgsParser Opt.<**> Opt.helper) Opt.fullDesc

    newProjectArgsParser :: Opt.Parser NewProjectArgs
    newProjectArgsParser =
      NewProjectArgs
        <$> Opt.optional projectNameParser
        <*> Opt.optional templateNameParser
        <*> forceFallbackTemplateParser

    projectNameParser :: Opt.Parser String
    projectNameParser = Opt.strArgument $ Opt.metavar "PROJECT_NAME"

    templateNameParser :: Opt.Parser String
    templateNameParser =
      Opt.strOption $
        Opt.long "template"
          <> Opt.short 't'
          <> Opt.metavar "TEMPLATE_NAME"
          <> Opt.help "Template to use for the new project"

    forceFallbackTemplateParser :: Opt.Parser Bool
    forceFallbackTemplateParser =
      Opt.switch $
        Opt.long "force-fallback-template"
          <> Opt.help "Forces the use of fallback template, even if there are remote templates available."

    parserResultToNewProjectArgs :: Opt.ParserResult NewProjectArgs -> Either String NewProjectArgs
    parserResultToNewProjectArgs (Opt.Success success) = Right success
    parserResultToNewProjectArgs (Opt.Failure failure) = Left $ show help
      where
        (help, _, _) = Opt.execFailure failure "wasp new"
    parserResultToNewProjectArgs (Opt.CompletionInvoked _) = Left "Completion invoked when parsing 'wasp new', but this should never happen"
