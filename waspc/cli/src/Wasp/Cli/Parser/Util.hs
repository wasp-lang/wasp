module Wasp.Cli.Parser.Util (mkNormalCommand, mkCommand) where

import Options.Applicative (CommandFields, InfoMod, Mod, Parser)
import qualified Options.Applicative as O

mkCommand :: String -> [InfoMod a] -> Parser a -> Mod CommandFields a
mkCommand name infoModifiers commandCall =
  O.command
    name
    ( O.info
        (O.helper <*> commandCall)
        (mconcat infoModifiers)
    )

mkNormalCommand :: String -> String -> Parser a -> Mod CommandFields a
mkNormalCommand name description commandCall = mkCommand name [O.progDesc description] commandCall
