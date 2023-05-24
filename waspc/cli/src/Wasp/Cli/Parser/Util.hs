module Wasp.Cli.Parser.Util (mkCommand, mkWrapperCommand) where

import Options.Applicative (InfoMod, Parser)
import qualified Options.Applicative as O

mkWrapperCommand :: String -> [InfoMod a] -> Parser a -> String -> O.Mod O.CommandFields a
mkWrapperCommand name infoModifiers callCommand description =
  O.command
    name
    ( O.info
        (O.helper <*> callCommand)
        (O.progDesc description <> mconcat infoModifiers)
    )

mkCommand :: String -> Parser a -> String -> O.Mod O.CommandFields a
mkCommand name callCommand description = mkWrapperCommand name [] callCommand description
