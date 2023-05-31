module Wasp.Cli.Command.Start.Parser (startParser) where

import Data.Maybe (fromMaybe)
import Options.Applicative
  ( Parser,
  )
import qualified Options.Applicative as O
import Wasp.Cli.Command.Call (CommandCall (Start), StartArg (..))
import Wasp.Cli.Parser.Util (mkCommand)

startParser :: Parser CommandCall
startParser = Start <$> startArgParser
  where
    startArgParser = fromMaybe StartApp <$> startDbParser

startDbParser :: Parser (Maybe StartArg)
startDbParser =
  O.optional $
    O.subparser $
      mkCommand "db" "Starts managed development database for you." $ pure StartDb
