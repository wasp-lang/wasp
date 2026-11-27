module Tests.WaspBuildTest (test_waspBuild) where

import E2E
import qualified StrongPath as SP
import Tests.Fixtures (starterMainPageFile, userCodeTypeError, userCodeTypeErrorMessage)
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Project.Common (generatedAppDirInWaspProjectDir)

test_waspBuild :: TestTree
test_waspBuild =
  testGroup
    "wasp-build"
    [ test "fail-outside-project" $ waspFailsOutsideProject ["build"],
      test "fail-sqlite-project"
        $ withNewWaspProject minimalStarterTemplate
        $ waspFails ["build"] >>= assertOutputContains "SQLite (a default database) is not supported in production",
      test "succeed-postgresql-project" $
        withNewWaspProject minimalStarterTemplate $ do
          setDbToPostgres
          wasp ["build"] >>= assertOutputContains "Your wasp project has been successfully built!"
          -- `wasp new` already creates `.wasp` and `node_modules`, so we check
          -- for what only building creates.
          assertDirectoryExists $ SP.fromRelDir generatedAppDirInWaspProjectDir,
      test "fail-type-error" $
        withNewWaspProject minimalStarterTemplate $ do
          setDbToPostgres
          appendToFile starterMainPageFile userCodeTypeError
          waspFails ["build"] >>= assertOutputContains userCodeTypeErrorMessage
    ]
