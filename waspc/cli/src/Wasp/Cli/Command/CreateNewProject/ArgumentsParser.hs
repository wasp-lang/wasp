module Wasp.Cli.Command.CreateNewProject.ArgumentsParser
  ( NewProjectArgs (..),
    newProjectArgsParser,
  )
where

import Data.List (intercalate)
import qualified Options.Applicative as Opt
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (availableStarterTemplates)

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
          <> Opt.help
            ( "Template to use for the new project. Available templates: "
                <> intercalate ", " (map show availableStarterTemplates)
                <> "."
            )
