module Wasp.Cli.Parser.Util (CommandType (..), mkCommand, mkWrapperCommand) where

import Options.Applicative (Parser)
import qualified Options.Applicative as O

data CommandType = CTNormal | CTNoIntersperse | CTForwardOptions

mkWrapperCommand :: String -> CommandType -> Parser a -> String -> O.Mod O.CommandFields a
mkWrapperCommand name commandType callCommand description =
  O.command
    name
    ( O.info
        (O.helper <*> callCommand)
        (O.progDesc description <> withModifier commandType)
    )
  where
    withModifier :: CommandType -> O.InfoMod a
    withModifier CTNormal = O.progDesc ""
    withModifier CTNoIntersperse = O.noIntersperse
    withModifier CTForwardOptions = O.forwardOptions

mkCommand :: String -> Parser a -> String -> O.Mod O.CommandFields a
mkCommand name callCommand description = mkWrapperCommand name CTNormal callCommand description
