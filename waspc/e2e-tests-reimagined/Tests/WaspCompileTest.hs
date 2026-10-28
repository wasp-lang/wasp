module Tests.WaspCompileTest (waspCompileTests) where

import E2e
import Tests.Fixtures (writeTasksQuery, writeTasksQueryApp)
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

waspCompileTests :: [E2eTest]
waspCompileTests =
  testGroup
    "wasp-compile"
    [ test "fail-outside-project" $ waspFails ["compile"],
      test "succeed-uncompiled-project" $
        withNewWaspProject minimalStarterTemplate $ do
          wasp ["compile"]
          assertDirectoryExists ".wasp"
          assertDirectoryExists "node_modules",
      test "succeed-compiled-project" $
        withNewWaspProject minimalStarterTemplate $ do
          wasp ["compile"]
          wasp ["compile"]
          assertDirectoryExists ".wasp"
          assertDirectoryExists "node_modules",
      -- Regression test for https://github.com/wasp-lang/wasp/issues/2001
      test "fail-missing-side-effect-import" $
        withNewWaspProject minimalStarterTemplate $ do
          appendToFile "src/MainPage.tsx" "import './missing-side-effect-import'"
          waspFails ["compile"]
            >>= assertOutputContains "Cannot find module or type declarations for side-effect import of './missing-side-effect-import'.",
      test "fail-type-error" $
        withNewWaspProject minimalStarterTemplate $ do
          writeToFile "src/utils.ts" "export const shouldBeNumber: number = 'wrong'"
          waspFails ["compile"]
            >>= assertOutputContains "src/utils.ts(1,14): error TS2322: Type 'string' is not assignable to type 'number'.",
      -- Regression test for https://github.com/wasp-lang/wasp/pull/4942
      test "fail-and-recover-with-updated-operation-types" $
        withNewWaspProject minimalStarterTemplate $ do
          writeTasksQueryApp
          wasp ["compile"]
          writeTasksQuery "[]"
          waspFails ["compile"] >>= assertOutputContains "Property 'id' does not exist on type 'never'."
          writeTasksQuery "[{ id: 1 }]"
          wasp ["compile"]
    ]
