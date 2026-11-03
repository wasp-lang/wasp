module Tests.IncrementalTypeCheckingTest (incrementalTypeCheckingTest) where

import E2E
import Tests.Fixtures (writeTasksQuery, writeTasksQueryApp)
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
          writeTasksQuery "[]"
          -- Editing a file makes TypeScript report the initial error.
          appendToFile "src/MainPage.tsx" ""
          -- Compile rejects the type error, but still generates the new types.
          waspFails ["compile"]
          runExpectingFailure typeCheckApp >>= assertOutputContains "Property 'id' does not exist on type 'never'."
          writeTasksQuery "[{ id: 1 }]"
          wasp ["compile"]
          run typeCheckApp
    ]
  where
    typeCheckApp :: Command
    typeCheckApp = nodeModulesBin "tsc" ["--build", "tsconfig.src.json"]
