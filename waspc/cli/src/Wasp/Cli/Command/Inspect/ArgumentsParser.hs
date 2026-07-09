module Wasp.Cli.Command.Inspect.ArgumentsParser
  ( InspectArgs (..),
    inspectArgsParser,
  )
where

import qualified Options.Applicative as Opt

newtype InspectArgs = InspectArgs
  { json :: Bool
  }

inspectArgsParser :: Opt.Parser InspectArgs
inspectArgsParser =
  InspectArgs
    <$> Opt.switch
      ( Opt.long "json"
          <> Opt.help
            "Print the full evaluated app spec as JSON. The schema follows Wasp's \
            \internal spec format and may change between Wasp versions."
      )
