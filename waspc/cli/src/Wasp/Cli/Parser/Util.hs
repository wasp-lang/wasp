module Wasp.Cli.Parser.Util (mkCommand, mkWrapperCommand) where

import Options.Applicative (Parser)
import qualified Options.Applicative as O

mkCommand :: String -> Parser a -> String -> O.Mod O.CommandFields a
mkCommand name callCommand description =
  O.command name (O.info (O.helper <*> callCommand) (O.progDesc description))

mkWrapperCommand :: String -> Parser a -> String -> O.Mod O.CommandFields a
mkWrapperCommand name callCommand description =
  O.command name (O.info (O.helper <*> callCommand) (O.progDesc description <> O.forwardOptions))
