module Wasp.Cli.Util.PortArgument
  ( portParser,
    parsePort,
  )
where

import Network.Socket (PortNumber)
import qualified Options.Applicative as Opt
import Text.Read (readMaybe)

portParser :: String -> String -> Opt.Parser PortNumber
portParser optionName helpText =
  Opt.option
    (Opt.eitherReader parsePort)
    ( Opt.long optionName
        <> Opt.metavar "PORT"
        <> Opt.help helpText
    )

-- | Reading into a 'PortNumber' already rejects anything outside 0-65535. We
-- also reject 0, which means "let the OS pick a port". We can't work with that,
-- since we have to tell the other side where this one is running.
parsePort :: String -> Either String PortNumber
parsePort input = case readMaybe input of
  Just 0 -> Left "0 is not a valid port"
  Just port -> Right port
  Nothing -> Left $ show input ++ " is not a valid port"
