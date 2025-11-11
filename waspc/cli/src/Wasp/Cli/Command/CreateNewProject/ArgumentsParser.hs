module Wasp.Cli.Command.CreateNewProject.ArgumentsParser
  ( NewProjectArgs (..),
    newProjectArgsParser,
    NewCustomProjectArgs (..),
    newCustomProjectArgsParser,
  )
where

import qualified Options.Applicative as Opt
import Wasp.Cli.Util.PathArgument (DirPathArgument, dirPathReader)

data NewProjectArgs = NewProjectArgs
  { _projectName :: Maybe String,
    _templateName :: Maybe String
  }

newProjectArgsParser :: Opt.Parser NewProjectArgs
newProjectArgsParser =
  NewProjectArgs
    <$> Opt.optional projectNameParser
    <*> Opt.optional templateNameParser
  where
    projectNameParser :: Opt.Parser String
    projectNameParser = Opt.strArgument $ Opt.metavar "PROJECT_NAME"

    templateNameParser :: Opt.Parser String
    templateNameParser =
      Opt.strOption $
        Opt.long "template"
          <> Opt.short 't'
          <> Opt.metavar "TEMPLATE_NAME"
          <> Opt.help "Template to use for the new project"

data NewCustomProjectArgs = NewCustomProjectArgs
  { _customProjectName :: Maybe String,
    _customTemplatePath :: DirPathArgument
  }

newCustomProjectArgsParser :: Opt.Parser NewCustomProjectArgs
newCustomProjectArgsParser =
  NewCustomProjectArgs
    <$> Opt.optional projectNameParser
    <*> templateNameParser
  where
    projectNameParser :: Opt.Parser String
    projectNameParser = Opt.strArgument $ Opt.metavar "PROJECT_NAME"

    templateNameParser :: Opt.Parser DirPathArgument
    templateNameParser =
      Opt.option dirPathReader $
        Opt.long "path"
          <> Opt.short 'p'
          <> Opt.metavar "TEMPLATE_PATH"
          <> Opt.help "Path to the local directory to use as a starter template"
