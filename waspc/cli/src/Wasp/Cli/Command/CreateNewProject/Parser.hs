module Wasp.Cli.Command.CreateNewProject.Parser (new) where

import Options.Applicative
  ( CommandFields,
    Mod,
    Parser,
    help,
    long,
    metavar,
    optional,
    short,
    strArgument,
    strOption,
  )
import Wasp.Cli.Command.Call (Call (New), NewArgs (NewArgs))
import Wasp.Cli.Parser.Util (mkCommand)

new :: Mod CommandFields Call
new =
  mkCommand
    "new"
    parseNew
    "Creates a new Wasp project. Run it without arguments for interactive mode."

parseNew :: Parser Call
parseNew = New <$> parseNewArgs
  where
    parseNewArgs =
      NewArgs
        <$> optional parseProjectName
        <*> optional parseTemplateName

parseTemplateName :: Parser String
parseTemplateName =
  strOption $
    long "template"
      <> short 't'
      <> metavar "TEMPLATE_NAME"
      <> help "Template to use for the new project"

parseProjectName :: Parser String
parseProjectName = strArgument $ metavar "PROJECT_NAME"
