module Wasp.Cli.Command.CreateNewProject.ArgumentsParser
  ( NewProjectArgs (..),
    NewProjectTemplateArg (..),
    newProjectArgsParser,
  )
where

import Control.Applicative (optional, (<|>))
import qualified Options.Applicative as Opt
import Wasp.Cli.Util.PathArgument (DirPathArgument)

data NewProjectArgs = NewProjectArgs
  { _projectName :: Maybe String,
    _templateArg :: Maybe NewProjectTemplateArg
  }

data NewProjectTemplateArg
  = NamedTemplateArg String
  | CustomTemplateDirArg DirPathArgument

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

    templateArgParser :: Opt.Parser NewProjectTemplateArg
    templateArgParser =
      (NamedTemplateArg <$> templateNameParser)
        <|> (CustomTemplateDirArg <$> customTemplatePathParser)

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
          <> Opt.internal
