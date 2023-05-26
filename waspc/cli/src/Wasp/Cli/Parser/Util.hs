module Wasp.Cli.Parser.Util (mkNormalCommand, mkCommand) where

import Options.Applicative (InfoMod, Parser)
import qualified Options.Applicative as O

mkCommand :: String -> [InfoMod a] -> Parser a -> String -> O.Mod O.CommandFields a
mkCommand name infoModifiers commandCall description =
  O.command
    name
    ( O.info
        (O.helper <*> commandCall)
        (O.progDesc description <> mconcat infoModifiers)
    )

mkNormalCommand :: String -> Parser a -> String -> O.Mod O.CommandFields a
mkNormalCommand name commandCall description = mkCommand name [] commandCall description
