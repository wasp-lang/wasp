module Tests.IncrementalTypeCheckingTest (incrementalTypeCheckingTest) where

import E2E
import Tests.Fixtures (breakTasksQueryType, fixTasksQueryType, tasksQueryTypeError, writeTasksQueryApp)
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

incrementalTypeCheckingTest :: E2ETest
incrementalTypeCheckingTest =
  testGroup
    "incremental-type-checking"
    [ -- Regression test for https://github.com/wasp-lang/wasp/pull/4885
      test "detects-and-recovers-operation-return-type-errors-incrementally" $
        withNewWaspProject minimalStarterTemplate $ do
          writeTasksQueryApp
          wasp ["compile"]
          run typeCheckApp
          breakTasksQueryType
          -- Touches MainPage.tsx without changing it (appends nothing), like
          -- saving it in an editor. Editing a file is what made TypeScript
          -- report the initial error.
          appendToFile "src/MainPage.tsx" ""
          -- Compile rejects the type error, but still generates the new types.
          waspFails ["compile"] >>= assertOutputContains tasksQueryTypeError
          runExpectingFailure typeCheckApp >>= assertOutputContains tasksQueryTypeError
          fixTasksQueryType
          wasp ["compile"]
          run typeCheckApp
    ]
  where
    typeCheckApp :: Command
    typeCheckApp = nodeModulesBin "tsc" ["--build", "tsconfig.src.json"]
