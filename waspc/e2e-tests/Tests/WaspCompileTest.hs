module Tests.WaspCompileTest (waspCompileTest) where

import E2E
import qualified StrongPath as SP
import Tests.Fixtures (writeTasksQuery, writeTasksQueryApp)
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Project.Common (generatedAppDirInWaspProjectDir)

waspCompileTest :: E2ETest
waspCompileTest =
  testGroup
    "wasp-compile"
    [ test "fail-outside-project" $ waspFails ["compile"],
      test "succeed-uncompiled-project" $
        withNewWaspProject minimalStarterTemplate $ do
          wasp ["compile"]
          assertDirectoryExists generatedAppDir,
      test "succeed-compiled-project" $
        withNewWaspProject minimalStarterTemplate $ do
          wasp ["compile"]
          wasp ["compile"]
          assertDirectoryExists generatedAppDir,
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
  where
    -- `wasp new` already creates `.wasp` and `node_modules`, so we check for
    -- what only compiling creates.
    generatedAppDir = SP.fromRelDir generatedAppDirInWaspProjectDir
