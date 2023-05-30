module Wasp.Cli.Command.Start.Parser where

import Data.Maybe (fromMaybe)
import Options.Applicative
  ( CommandFields,
    Mod,
    Parser,
  )
import qualified Options.Applicative as O
import Wasp.Cli.Command.Call (CommandCall (Start), StartArg (..))
import Wasp.Cli.Parser.Util (mkNormalCommand)

start :: Mod CommandFields CommandCall
start =
  mkNormalCommand
    "start"
    "Runs Wasp app in development mode, watching for file changes."
    parseStart

parseStart :: Parser CommandCall
parseStart = Start <$> parseStartArg
  where
    parseStartArg = fromMaybe StartApp <$> parseStartDb

parseStartDb :: Parser (Maybe StartArg)
parseStartDb =
  O.optional $
    O.subparser $
      mkNormalCommand "db" "Starts managed development database for you." $ pure StartDb
