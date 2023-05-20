module Wasp.Cli.Command.Start.Parser where

import Data.Maybe (fromMaybe)
import Options.Applicative
  ( CommandFields,
    Mod,
    Parser,
  )
import qualified Options.Applicative as O
import Wasp.Cli.Command.Call (Call (Start), StartArg (..))
import Wasp.Cli.Parser.Util (mkCommand)

start :: Mod CommandFields Call
start =
  mkCommand
    "start"
    parseStart
    "Runs Wasp app in development mode, watching for file changes."

parseStart :: Parser Call
parseStart = Start <$> parseStartArg
  where
    parseStartArg = fromMaybe StartNormal <$> parseStartDb

parseStartDb :: Parser (Maybe StartArg)
parseStartDb =
  O.optional $
    O.subparser $
      mkCommand "db" (pure StartDb) "This will launch the PostgreSQL's development database."
