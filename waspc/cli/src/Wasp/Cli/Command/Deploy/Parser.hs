module Wasp.Cli.Command.Deploy.Parser (deployParser) where

import Options.Applicative
  ( Parser,
  )
import qualified Options.Applicative as O
import Wasp.Cli.Command.Call (CommandCall (Deploy))

deployParser :: Parser CommandCall
deployParser = Deploy <$> O.many (O.strArgument mempty)
