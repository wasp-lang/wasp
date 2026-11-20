module Tests.VitestTest (vitestTest) where

import Control.Monad.Reader (ask)
import qualified Data.Text as T
import NeatInterpolation (trimming)
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    WaspProjectContext (..),
    createTestWaspProject,
    inTestWaspProjectDir,
    replaceMainWaspTsFile,
    waspCliCompile,
    writeToFile,
  )
import StrongPath (File', Path', Rel, relfile, (</>))
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Project.Common (WaspProjectDir)

-- | Sets up Vitest in a Wasp app the way the testing docs describe, and runs
-- client tests that use the \@wasp/test-utils helpers.
vitestTest :: Test
vitestTest =
  Test
    "vitest"
    [ TestCase
        "runs-client-tests"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ replaceMainWaspTsFile mainWaspTs,
                  writeProjectFile [relfile|src/queries.ts|] queriesTs,
                  writeProjectFile [relfile|src/TaskList.tsx|] taskListTsx,
                  writeProjectFile [relfile|vite.config.ts|] viteConfigTs,
                  -- The app must compile without the testing libraries.
                  waspCliCompile,
                  addDevWaspTestUtilsPackage,
                  installTestingLibraries,
                  -- Wasp type-checks the tests too, so we add them once their
                  -- dependencies are installed.
                  writeProjectFile [relfile|src/TaskList.test.tsx|] taskListTestTsx,
                  waspCliCompile,
                  return "npx vitest run"
                ]
            ]
        )
    ]
  where
    -- A user installs \@wasp/test-utils from npm. The dev one isn't there, so
    -- the project depends on its tarball, which `wasp new` already put into
    -- the `overrides`. npm wants the same specifier in both places.
    addDevWaspTestUtilsPackage :: ShellCommandBuilder WaspProjectContext ShellCommand
    addDevWaspTestUtilsPackage =
      return "npm pkg set \"devDependencies.@wasp/test-utils=$(jq -r '.overrides[\"@wasp/test-utils\"]' package.json)\""

    installTestingLibraries :: ShellCommandBuilder WaspProjectContext ShellCommand
    installTestingLibraries = return "npm install -D vitest jsdom @testing-library/react msw"

    writeProjectFile :: Path' (Rel WaspProjectDir) File' -> T.Text -> ShellCommandBuilder WaspProjectContext ShellCommand
    writeProjectFile fileInProject content = do
      context <- ask
      writeToFile (context.waspProjectDir </> fileInProject) content

    mainWaspTs :: T.Text
    mainWaspTs =
      [trimming|
        import { app, page, query, route } from "@wasp/spec";
        import { MainPage } from "./src/MainPage" with { type: "ref" };
        import { getTasks } from "./src/queries" with { type: "ref" };

        export default app({
          name: "vitestPackage",
          title: "Vitest package",
          spec: [
            route("RootRoute", "/", page(MainPage)),
            query(getTasks),
          ],
        });
      |]

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
        import { mockServer, renderInContext } from "@wasp/test-utils";
        import { expect, test } from "vitest";
        import { HttpMethod } from "wasp/client";
        import { getTasks } from "wasp/client/operations";
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
        import { waspTestConfig } from "@wasp/test-utils/config";
        import { defineConfig } from "vitest/config";
        import { wasp } from "wasp/client/vite";

        export default defineConfig({
          plugins: [wasp()],
          test: waspTestConfig(),
        });
      |]
