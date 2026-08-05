module Wasp.Cli.Command.Show.Spec
  ( showSpec,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import Wasp.AppSpec.Core.Inspectable (inspect)
import Wasp.AppSpec.Inspectable (InspectableAppSpec (InspectableAppSpec))
import Wasp.Cli.Command (Command, require)
import Wasp.Cli.Command.Compile (analyzeWithDiagnosticsOnStderr)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import Wasp.Cli.Command.Require.WaspSpecAvailable (WaspSpecAvailable (WaspSpecAvailable))
import Wasp.Cli.Command.Show.ArgumentsParser (ShowSubcommandArgs (..))
import Wasp.Cli.Command.Show.Table (renderEntriesAsTables)
import Wasp.Version (waspVersion)

-- | Prints the evaluated app spec: as a human-readable overview by default, or
-- as full JSON with --json.
showSpec :: ShowSubcommandArgs -> Command ()
showSpec args = do
  ValidNodeAndNpm <- require
  InWaspProject waspDir <- require
  WaspSpecAvailable <- require
  appSpec <- analyzeWithDiagnosticsOnStderr waspDir
  liftIO $ putStr $ renderFn args.json appSpec
  where
    renderFn isJson
      | isJson = specAsJson
      | otherwise = specAsTables

specAsTables :: AppSpec -> String
specAsTables = renderEntriesAsTables . inspect . InspectableAppSpec

specAsJson :: AppSpec -> String
specAsJson appSpec =
  BSL8.unpack $ encodePretty outputObject <> "\n"
  where
    outputObject =
      object
        [ "waspVersion" .= show waspVersion,
          "decls" .= AS.decls appSpec
        ]
