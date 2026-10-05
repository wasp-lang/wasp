module Wasp.Cli.Command.Show.Spec
  ( specShowSubcommand,
  )
where

import Wasp.AppSpec.Inspectable (InspectableAppSpec (InspectableAppSpec))
import Wasp.Cli.Command (Command, require)
import Wasp.Cli.Command.Compile (analyzeWithDiagnosticsOnStderr)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import Wasp.Cli.Command.Require.WaspSpecAvailable (WaspSpecAvailable (WaspSpecAvailable))
import Wasp.Cli.Command.Show.Subcommand (ShowSubcommand (..))

specShowSubcommand :: ShowSubcommand
specShowSubcommand =
  ShowSubcommand
    { name = "spec",
      description = "Prints an overview of your app: routes, pages, queries, actions, and more",
      getData = analyzeAppSpec
    }

analyzeAppSpec :: Command InspectableAppSpec
analyzeAppSpec = do
  ValidNodeAndNpm <- require
  InWaspProject waspDir <- require
  WaspSpecAvailable <- require
  InspectableAppSpec <$> analyzeWithDiagnosticsOnStderr waspDir
