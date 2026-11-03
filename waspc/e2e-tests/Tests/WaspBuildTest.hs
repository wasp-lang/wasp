module Tests.WaspBuildTest (waspBuildTest) where

import E2e
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

waspBuildTest :: E2eTest
waspBuildTest =
  testGroup
    "wasp-build"
    [ test "fail-outside-project" $ waspFails ["build"],
      test "fail-sqlite-project"
        $ withNewWaspProject minimalStarterTemplate
        $ waspFails ["build"],
      test "succeed-postgresql-project" $
        withNewWaspProject minimalStarterTemplate $ do
          setDbToPostgres
          wasp ["build"]
          assertDirectoryExists ".wasp"
          assertDirectoryExists "node_modules",
      test "fail-type-error" $
        withNewWaspProject minimalStarterTemplate $ do
          setDbToPostgres
          appendToFile "src/MainPage.tsx" "export const shouldBeNumber: number = 'wrong'"
          waspFails ["build"] >>= assertOutputContains "error TS2322: Type 'string' is not assignable to type 'number'."
    ]
