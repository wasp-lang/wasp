module Tests.IncrementalTypeCheckingTest (incrementalTypeCheckingTests) where

import E2e
import Tests.Fixtures (writeTasksQuery, writeTasksQueryApp)
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

incrementalTypeCheckingTests :: [E2eTest]
incrementalTypeCheckingTests =
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
    typeCheckApp = command "npx" ["tsc", "--build", "tsconfig.src.json"]
