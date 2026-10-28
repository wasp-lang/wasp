module Tests.WaspShowTest (waspShowTests) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import E2e
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

waspShowTests :: [E2eTest]
waspShowTests =
  testGroup
    "wasp-show"
    [ test "fail-without-subcommand" $ waspFails ["show"],
      test "fail-outside-project" $ do
        waspFails ["show", "spec"]
        waspFails ["show", "build"],
      test "succeed-inside-project" $
        withNewWaspProject minimalStarterTemplate $ do
          wasp ["show", "spec"]
          -- Stdout alone must be valid JSON, so that it can be piped to other tools.
          specJson <- wasp ["show", "spec", "--json"] >>= assertValidJson . (.stdout)
          assertTrue "`wasp show spec --json` has the {waspVersion, decls} envelope" (hasKeys ["waspVersion", "decls"] specJson)
          wasp ["show", "build"]
          wasp ["show", "build", "--json"] >>= assertValidJson . (.stdout)
    ]
  where
    hasKeys keys = \case
      Aeson.Object object -> all (`KeyMap.member` object) keys
      _ -> False
