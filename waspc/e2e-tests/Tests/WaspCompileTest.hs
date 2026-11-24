module Tests.WaspCompileTest (waspCompileTest) where

import E2E
import qualified StrongPath as SP
import Tests.Fixtures (breakTasksQueryType, fixTasksQueryType, tasksQueryTypeError, userCodeTypeError, userCodeTypeErrorMessage, writeTasksQueryApp)
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Project.Common (generatedAppDirInWaspProjectDir)

waspCompileTest :: E2ETest
waspCompileTest =
  testGroup
    "wasp-compile"
    [ test "fail-outside-project" $ waspFailsOutsideProject ["compile"],
      test "succeed-uncompiled-project" $
        withNewWaspProject minimalStarterTemplate $ do
          waspCompileSucceeds
          assertDirectoryExists generatedAppDir,
      test "succeed-compiled-project" $
        withNewWaspProject minimalStarterTemplate $ do
          waspCompileSucceeds
          waspCompileSucceeds
          assertDirectoryExists generatedAppDir,
      -- Regression test for https://github.com/wasp-lang/wasp/issues/2001
      test "fail-missing-side-effect-import" $
        withNewWaspProject minimalStarterTemplate $ do
          appendToFile "src/MainPage.tsx" "import './missing-side-effect-import'"
          waspFails ["compile"]
            >>= assertOutputContains "Cannot find module or type declarations for side-effect import of './missing-side-effect-import'.",
      test "fail-type-error" $
        withNewWaspProject minimalStarterTemplate $ do
          writeToFile "src/utils.ts" userCodeTypeError
          waspFails ["compile"] >>= assertOutputContains ("src/utils.ts(1,14): " <> userCodeTypeErrorMessage),
      -- Regression test for https://github.com/wasp-lang/wasp/pull/4942
      test "fail-and-recover-with-updated-operation-types" $
        withNewWaspProject minimalStarterTemplate $ do
          writeTasksQueryApp
          wasp ["compile"]
          breakTasksQueryType
          waspFails ["compile"] >>= assertOutputContains tasksQueryTypeError
          fixTasksQueryType
          wasp ["compile"]
    ]
  where
    waspCompileSucceeds :: TestM ()
    waspCompileSucceeds = wasp ["compile"] >>= assertOutputContains "Your wasp project has successfully compiled."

    -- `wasp new` already creates `.wasp` and `node_modules`, so we check for
    -- what only compiling creates.
    generatedAppDir = SP.fromRelDir generatedAppDirInWaspProjectDir
