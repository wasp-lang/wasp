module Tests.WaspCompileTest (waspCompileTest) where

import Control.Monad (void)
import qualified Data.Text as T
import E2e
import NeatInterpolation (trimming)
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

waspCompileTest :: E2eTest
waspCompileTest =
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
          writeToFile "main.wasp.ts" mainWaspTsWithQuery
          writeQuery "[{ id: 1 }]"
          writeMainPageUsingQuery
          wasp ["compile"]
          writeQuery "[]"
          waspFails ["compile"] >>= assertOutputContains "Property 'id' does not exist on type 'never'."
          writeQuery "[{ id: 1 }]"
          void $ wasp ["compile"]
    ]
  where
    writeQuery :: T.Text -> TestM ()
    writeQuery result =
      writeToFile
        "src/queries.ts"
        [trimming|
          import type { GetTasks } from "wasp/server/operations";

          export const getTasks = (async () => {
            return $result;
          }) satisfies GetTasks<void>;
        |]

    writeMainPageUsingQuery :: TestM ()
    writeMainPageUsingQuery =
      writeToFile
        "src/MainPage.tsx"
        [trimming|
          import { getTasks, useQuery } from "wasp/client/operations";

          export function MainPage() {
            const { data: tasks } = useQuery(getTasks);
            return <div>{tasks?.map((task) => <p key={task.id}>{task.id}</p>)}</div>;
          }
        |]

    mainWaspTsWithQuery :: T.Text
    mainWaspTsWithQuery =
      [trimming|
        import { app, page, query, route } from "@wasp/spec";
        import { MainPage } from "./src/MainPage" with { type: "ref" };
        import { getTasks } from "./src/queries" with { type: "ref" };

        export default app({
          name: "operationTypes",
          title: "Operation types",
          wasp: { version: "$textWaspVersion" },
          spec: [
            route("RootRoute", "/", page(MainPage)),
            query(getTasks),
          ],
        });
      |]
