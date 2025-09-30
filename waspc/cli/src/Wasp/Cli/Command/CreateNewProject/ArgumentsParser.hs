module Wasp.Cli.Command.CreateNewProject.ArgumentsParser where

import Data.List (intercalate)
import qualified Options.Applicative as Opt
import Wasp.Cli.Command.CreateNewProject.StarterTemplates (availableStarterTemplates)

data NewProjectArgs = NewProjectArgs
  { _projectName :: Maybe String,
    _templateName :: Maybe String
  }
  deriving (Show)

newProjectArgsParserInfo :: Opt.ParserInfo NewProjectArgs
newProjectArgsParserInfo =
  Opt.info
    newProjectArgsParser
    $ Opt.fullDesc
      <> Opt.progDesc "Creates a new Wasp project. Run it without arguments for interactive mode."

newProjectArgsParser :: Opt.Parser NewProjectArgs
newProjectArgsParser =
  NewProjectArgs
    <$> Opt.optional projectNameParser
    <*> Opt.optional templateNameParser
  where
    projectNameParser :: Opt.Parser String
    projectNameParser =
      Opt.strArgument $
        Opt.metavar "PROJECT_NAME"
          <> Opt.help "Name of the new Wasp project. The project will be created in a new directory with this name."

    templateNameParser :: Opt.Parser String
    templateNameParser =
      Opt.strOption $
        Opt.long "template"
          <> Opt.short 't'
          <> Opt.metavar "TEMPLATE_NAME"
          <> Opt.help ("Template to use for the new project (" ++ intercalate ", " availableTemplateNames ++ ")")
          <> Opt.completeWith availableTemplateNames

    availableTemplateNames = show <$> availableStarterTemplates
