module Wasp.Cli.Parser.Util (mkCommand, mkCommandWithInfoModifiers) where

import Options.Applicative (CommandFields, InfoMod, Mod, Parser)
import qualified Options.Applicative as O

mkCommand :: String -> String -> Parser a -> Mod CommandFields a
mkCommand name description commandParser =
  mkCommandWithInfoModifiers name [O.progDesc description] commandParser

mkCommandWithInfoModifiers :: String -> [InfoMod a] -> Parser a -> Mod CommandFields a
mkCommandWithInfoModifiers name infoModifiers commandParser =
  O.command
    name
    ( O.info
        (O.helper <*> commandParser)
        (mconcat infoModifiers)
    )
