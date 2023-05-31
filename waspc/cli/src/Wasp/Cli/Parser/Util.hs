module Wasp.Cli.Parser.Util (mkCommand, mkCommandWithInfo) where

import Options.Applicative (CommandFields, InfoMod, Mod, Parser)
import qualified Options.Applicative as O

mkCommand :: String -> String -> Parser a -> Mod CommandFields a
mkCommand name description commandParser =
  mkCommandWithInfo name [O.progDesc description] commandParser

mkCommandWithInfo :: String -> [InfoMod a] -> Parser a -> Mod CommandFields a
mkCommandWithInfo name infoModifiers commandParser =
  O.command
    name
    ( O.info
        (O.helper <*> commandParser)
        (mconcat infoModifiers)
    )
