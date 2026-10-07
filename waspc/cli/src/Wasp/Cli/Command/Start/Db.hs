module Wasp.Cli.Command.Start.Db
  ( start,
  )
where

import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Call (Arguments)
import qualified Wasp.Cli.Command.Db.DevDb as DevDb
import Wasp.Cli.Command.Db.StartOptions (dbStartOptionsParser)
import Wasp.Cli.Util.Parser (withArguments)

start :: Arguments -> Command ()
start = withArguments "wasp db start" dbStartOptionsParser DevDb.start
