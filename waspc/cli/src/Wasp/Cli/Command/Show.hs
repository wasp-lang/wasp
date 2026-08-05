module Wasp.Cli.Command.Show
  ( showCommand,
  )
where

import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.Show.ArgumentsParser (ShowArgs (..), showArgsParser)
import Wasp.Cli.Command.Show.Build (showBuild)
import Wasp.Cli.Command.Show.Spec (showSpec)
import Wasp.Cli.Util.Parser (withArguments)

-- | Prints information about the project: the evaluated app spec with
-- `wasp show spec`, or the current build with `wasp show build`.
showCommand :: Arguments -> Command ()
showCommand = withArguments "wasp show" showArgsParser $ \case
  ShowSpec args -> showSpec args
  ShowBuild args -> showBuild args
