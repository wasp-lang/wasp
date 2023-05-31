module Wasp.Cli.Command.CreateNewProject.Parser (newParser) where

import Options.Applicative
  ( Parser,
  )
import qualified Options.Applicative as O
import Wasp.Cli.Command.Call (CommandCall (New), NewProjectArgs (NewProjectArgs))

newParser :: Parser CommandCall
newParser = New <$> newArgsParser
  where
    newArgsParser =
      NewProjectArgs
        <$> O.optional projectNameParser
        <*> O.optional templateNameParser

templateNameParser :: Parser String
templateNameParser =
  O.strOption $
    O.long "template"
      <> O.short 't'
      <> O.metavar "TEMPLATE_NAME"
      <> O.help "Template to use for the new project"

projectNameParser :: Parser String
projectNameParser = O.strArgument $ O.metavar "PROJECT_NAME"
