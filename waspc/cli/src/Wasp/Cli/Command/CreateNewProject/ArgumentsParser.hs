module Wasp.Cli.Command.CreateNewProject.ArgumentsParser
  ( NewProjectArgs (..),
    TemplateArg (..),
    newProjectArgsParser,
  )
where

import Control.Applicative (optional, (<|>))
import qualified Options.Applicative as Opt
import Wasp.Cli.Util.PathArgument (DirPathArgument)

data NewProjectArgs = NewProjectArgs
  { _projectName :: Maybe String,
    _templateArg :: Maybe TemplateArg
  }

data TemplateArg
  = NamedTemplate String
  | CustomTemplate DirPathArgument

newProjectArgsParser :: Opt.Parser NewProjectArgs
newProjectArgsParser =
  NewProjectArgs
    <$> optional projectNameParser
    <*> optional templateArgParser
  where
    projectNameParser :: Opt.Parser String
    projectNameParser =
      Opt.strArgument $
        Opt.metavar "PROJECT_NAME"

    templateArgParser :: Opt.Parser TemplateArg
    templateArgParser =
      (NamedTemplate <$> templateNameParser)
        <|> (CustomTemplate <$> customTemplatePathParser)

    templateNameParser :: Opt.Parser String
    templateNameParser =
      Opt.strOption $
        Opt.long "template"
          <> Opt.short 't'
          <> Opt.metavar "TEMPLATE_NAME"
          <> Opt.help "Template to use for the new project"

    customTemplatePathParser :: Opt.Parser DirPathArgument
    customTemplatePathParser =
      Opt.strOption $
        Opt.long "custom-template"
          -- This is an internal option only intended for internal testing usage,
          -- so we don't want to show help for it.
          <> Opt.internal
