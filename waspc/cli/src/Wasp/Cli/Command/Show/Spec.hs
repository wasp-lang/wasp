module Wasp.Cli.Command.Show.Spec
  ( specShowSubcommand,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import Wasp.AppSpec.Core.Inspectable (inspect)
import Wasp.AppSpec.Inspectable (InspectableAppSpec (InspectableAppSpec))
import Wasp.Cli.Command (Command, require)
import Wasp.Cli.Command.Compile (analyzeWithDiagnosticsOnStderr)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import Wasp.Cli.Command.Require.WaspSpecAvailable (WaspSpecAvailable (WaspSpecAvailable))
import Wasp.Cli.Command.Show.Subcommand (ShowSubcommand (..))
import Wasp.Version (waspVersion)

-- | Shows the evaluated app spec: as a human-readable overview by default, or
-- as full JSON with --json.
specShowSubcommand :: ShowSubcommand
specShowSubcommand =
  ShowSubcommand
    { name = "spec",
      description = "Prints an overview of your app: routes, pages, queries, actions, and more",
      jsonHelp =
        "Print the full evaluated app spec as JSON. The schema follows Wasp's "
          <> "internal spec format and may change between Wasp versions.",
      getInspectionEntries = inspect . InspectableAppSpec <$> analyzeAppSpec,
      getJson = specAsJson <$> analyzeAppSpec
    }

analyzeAppSpec :: Command AppSpec
analyzeAppSpec = do
  ValidNodeAndNpm <- require
  InWaspProject waspDir <- require
  WaspSpecAvailable <- require
  analyzeWithDiagnosticsOnStderr waspDir

specAsJson :: AppSpec -> Aeson.Value
specAsJson appSpec =
  object
    [ "waspVersion" .= show waspVersion,
      "decls" .= AS.decls appSpec
    ]
