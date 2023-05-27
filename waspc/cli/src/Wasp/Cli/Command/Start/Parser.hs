module Wasp.Cli.Command.Start.Parser where

import Data.Maybe (fromMaybe)
import Options.Applicative
  ( CommandFields,
    Mod,
    Parser,
  )
import qualified Options.Applicative as O
import Wasp.Cli.Command.Call (Call (Start), StartArg (..))
import Wasp.Cli.Parser.Util (mkNormalCommand)

start :: Mod CommandFields Call
start =
  mkNormalCommand
    "start"
    "Runs Wasp app in development mode, watching for file changes."
    parseStart

parseStart :: Parser Call
parseStart = Start <$> parseStartArg
  where
    parseStartArg = fromMaybe StartApp <$> parseStartDb

parseStartDb :: Parser (Maybe StartArg)
parseStartDb =
  O.optional $
    O.subparser $
      mkNormalCommand "db" "Starts managed development database for you." $ pure StartDb
