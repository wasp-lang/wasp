module Wasp.Cli.Util.PortArgument
  ( portOption,
  )
where

import Network.Socket (PortNumber)
import qualified Options.Applicative as Opt

portOption :: String -> String -> Opt.Parser (Maybe PortNumber)
portOption optionName helpText =
  Opt.optional $
    Opt.option
      (Opt.auto >>= rejectAnyPort)
      ( Opt.long optionName
          <> Opt.metavar "PORT"
          <> Opt.help helpText
      )
  where
    -- Reading into a 'PortNumber' already rejects anything outside 1-65535,
    -- except for 0, which means "let the OS pick a port". We can't work with
    -- that, since we have to tell the other side where this one is running.
    rejectAnyPort 0 = Opt.readerError "0 is not a valid port"
    rejectAnyPort port = return port
