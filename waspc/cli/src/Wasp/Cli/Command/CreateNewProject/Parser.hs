module Wasp.Cli.Command.CreateNewProject.Parser (new) where

import Options.Applicative
  ( CommandFields,
    Mod,
    Parser,
  )
import qualified Options.Applicative as O
import Wasp.Cli.Command.Call (Call (New), NewProjectArgs (NewProjectArgs))
import Wasp.Cli.Parser.Util (mkNormalCommand)

new :: Mod CommandFields Call
new =
  mkNormalCommand
    "new"
    "Creates a new Wasp project. Run it without arguments for interactive mode."
    parseNew

parseNew :: Parser Call
parseNew = New <$> parseNewArgs
  where
    parseNewArgs =
      NewProjectArgs
        <$> O.optional parseProjectName
        <*> O.optional parseTemplateName

parseTemplateName :: Parser String
parseTemplateName =
  O.strOption $
    O.long "template"
      <> O.short 't'
      <> O.metavar "TEMPLATE_NAME"
      <> O.help "Template to use for the new project"

parseProjectName :: Parser String
parseProjectName = O.strArgument $ O.metavar "PROJECT_NAME"
