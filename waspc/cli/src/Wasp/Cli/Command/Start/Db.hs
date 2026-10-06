module Wasp.Cli.Command.Start.Db
  ( start,
  )
where

import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.Db.ArgumentsParser (startDbArgsParser)
import qualified Wasp.Cli.Command.Db.Lifecycle as DbLifecycle
import Wasp.Cli.Util.Parser (withArguments)

start :: Arguments -> Command ()
start = withArguments "wasp db start" startDbArgsParser DbLifecycle.start
