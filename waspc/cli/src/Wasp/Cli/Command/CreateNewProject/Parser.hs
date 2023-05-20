module Wasp.Cli.Command.CreateNewProject.Parser (new) where

import Options.Applicative
  ( CommandFields,
    Mod,
    Parser,
  )
import qualified Options.Applicative as O
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
