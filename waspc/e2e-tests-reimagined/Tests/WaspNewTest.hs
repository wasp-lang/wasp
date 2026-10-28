module Tests.WaspNewTest (waspNewTests) where

import qualified Data.Text as T
import E2e
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (basicStarterTemplate, minimalStarterTemplate)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates (StarterTemplate)

waspNewTests :: [E2eTest]
waspNewTests =
  testGroup
    "wasp-new"
    [ test "create-minimal" $ createWaspProject minimalStarterTemplate,
      test "create-minimal-interactive" $ createWaspProjectInteractively minimalStarterTemplate,
      test "create-basic" $ createWaspProject basicStarterTemplate,
      test "create-basic-interactive" $ createWaspProjectInteractively basicStarterTemplate
      -- NOTE: We can't test the `saas` template here, since its "dev" tag doesn't exist.
    ]
  where
    createWaspProjectInteractively :: StarterTemplate -> TestM ProcessResult
    createWaspProjectInteractively template =
      run . withStdinText (T.pack $ waspAppDirName ++ "\n" ++ show template ++ "\n") $ waspCli ["new"]
