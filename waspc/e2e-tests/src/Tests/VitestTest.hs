module Tests.VitestTest (test_vitest) where

import qualified Data.Text as T
import E2E
import NeatInterpolation (trimming)
import Tests.Fixtures (addTasksQueryToMainWaspTs)
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

-- | Sets up Vitest in a Wasp app the way the testing docs describe, and runs
-- client tests that use the `wasp/client/test` helpers.
test_vitest :: TestTree
test_vitest =
  testGroup
    "vitest"
    [ test "runs-client-tests" $
        withNewWaspProject minimalStarterTemplate $ do
          addTasksQueryToMainWaspTs
          writeToFile "src/queries.ts" queriesTs
          writeToFile "src/TaskList.tsx" taskListTsx
          writeToFile "vite.config.ts" viteConfigTs
          -- The SDK must compile without the testing libraries.
          wasp ["compile"]
          run installTestingLibrariesCommand
          -- Wasp type-checks the tests too, so we add them once their
          -- dependencies are installed.
          writeToFile "src/TaskList.test.tsx" taskListTestTsx
          wasp ["compile"]
          run $ nodeModulesBin "vitest" ["run"]
    ]
  where
    queriesTs :: T.Text
    queriesTs =
      [trimming|
        import type { GetTasks } from "wasp/server/operations";

        export const getTasks = (async ({ limit }, _context) => {
          return [{ id: 1, description: "From the server" }].slice(0, limit);
        }) satisfies GetTasks<{ limit: number }>;
      |]

    taskListTsx :: T.Text
    taskListTsx =
      [trimming|
        import { api } from "wasp/client/api";
        import { getTasks, useQuery } from "wasp/client/operations";
        import { useEffect, useState } from "react";

        export function TaskList() {
          const { data: tasks } = useQuery(getTasks, { limit: 10 });
          const [stats, setStats] = useState("");
          useEffect(() => {
            api.get("/stats").json<{ doneTasks: number }>().then((body) => {
              setStats(`$${body.doneTasks} done`);
            });
          }, []);

          return (
            <div>
              {tasks?.map((task) => <p key={task.id}>{task.description}</p>)}
              <p>{stats}</p>
            </div>
          );
        }
      |]

    taskListTestTsx :: T.Text
    taskListTestTsx =
      [trimming|
        import { screen } from "@testing-library/react";
        import { expect, test } from "vitest";
        import { HttpMethod } from "wasp/client";
        import { getTasks } from "wasp/client/operations";
        import { mockServer, renderInContext } from "wasp/client/test";
        import { TaskList } from "./TaskList";

        const { mockQuery, mockApi } = mockServer();

        test("renders mocked query and API data", async () => {
          mockQuery(getTasks, [{ id: 1, description: "Mocked task" }]);
          mockApi({ method: HttpMethod.Get, path: "/stats" }, { doneTasks: 3 });

          renderInContext(<TaskList />);

          expect(await screen.findByText("Mocked task")).toBeTruthy();
          expect(await screen.findByText("3 done")).toBeTruthy();
        });
      |]

    viteConfigTs :: T.Text
    viteConfigTs =
      [trimming|
        import { defineConfig } from "vitest/config";
        import { wasp } from "wasp/client/vite";
        import { waspTestConfig } from "wasp/test";

        export default defineConfig({
          plugins: [wasp()],
          test: waspTestConfig(),
        });
      |]

{- HLINT ignore installTestingLibrariesCommand "Avoid npm and npx" -}

-- | Like the testing docs, we install the latest versions, so that we notice
-- when a new release of these libraries breaks the `wasp/client/test` helpers.
installTestingLibrariesCommand :: Command
installTestingLibrariesCommand = command "npm" ["install", "-D", "vitest", "jsdom", "@testing-library/react", "msw"]
