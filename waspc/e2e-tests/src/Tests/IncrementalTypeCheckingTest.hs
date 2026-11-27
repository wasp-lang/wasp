module Tests.IncrementalTypeCheckingTest (test_incrementalTypeChecking) where

import E2E
import Tests.Fixtures (breakTasksQueryType, fixTasksQueryType, starterMainPageFile, tasksQueryTypeError, writeTasksQueryApp)
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

test_incrementalTypeChecking :: TestTree
test_incrementalTypeChecking =
  testGroup
    "incremental-type-checking"
    [ -- Regression test for https://github.com/wasp-lang/wasp/pull/4885
      test "detects-and-recovers-operation-return-type-errors-incrementally" $
        withNewWaspProject minimalStarterTemplate $ do
          writeTasksQueryApp
          wasp ["compile"]
          run typeCheckAppCommand
          breakTasksQueryType
          -- Touches MainPage.tsx by appending an empty line, like saving it in
          -- an editor. Editing a file is what made TypeScript report the
          -- initial error.
          appendToFile starterMainPageFile ""
          -- Compile rejects the type error, but still generates the new types.
          waspFails ["compile"] >>= assertOutputContains tasksQueryTypeError
          runExpectingFailure typeCheckAppCommand >>= assertOutputContains tasksQueryTypeError
          fixTasksQueryType
          wasp ["compile"]
          run typeCheckAppCommand
    ]
  where
    typeCheckAppCommand :: Command
    typeCheckAppCommand = nodeModulesBin "tsc" ["--build", "tsconfig.src.json"]
