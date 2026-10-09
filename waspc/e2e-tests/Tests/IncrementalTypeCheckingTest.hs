module Tests.IncrementalTypeCheckingTest (incrementalTypeCheckingTest) where

import Control.Monad.Reader (ask)
import qualified Data.Text as T
import NeatInterpolation (trimming)
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    WaspProjectContext (..),
    appendToFile,
    assertCommandOutputContains,
    createTestWaspProject,
    inTestWaspProjectDir,
    replaceMainWaspTsFile,
    waspCliCompile,
    writeToFile,
  )
import StrongPath (relfile, (</>))
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Version (waspVersion)

incrementalTypeCheckingTest :: Test
incrementalTypeCheckingTest =
  Test
    "incremental-type-checking"
    [ TestCase
        "detects-and-recovers-operation-return-type-errors-incrementally"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ replaceMainWaspTsFile mainWaspTs,
                  writeQuery "[{ id: 1 }]",
                  writeMainPage,
                  waspCliCompile,
                  typeCheckApp,
                  writeQuery "[]",
                  -- Editing a file makes TypeScript report the initial error.
                  appendToFile "src/MainPage.tsx" "",
                  -- Regression test for https://github.com/wasp-lang/wasp/pull/4942
                  -- Compile type-checks against the newly generated types.
                  assertCommandOutputContains
                    (("! " ++) <$> waspCliCompile)
                    "Property 'id' does not exist on type 'never'.",
                  -- Regression test for https://github.com/wasp-lang/wasp/pull/4885
                  -- Incremental type-checking doesn't keep stale diagnostics.
                  assertCommandOutputContains
                    (("! " ++) <$> typeCheckApp)
                    "Property 'id' does not exist on type 'never'.",
                  writeQuery "[{ id: 1 }]",
                  waspCliCompile,
                  typeCheckApp
                ]
            ]
        )
    ]
  where
    typeCheckApp :: ShellCommandBuilder WaspProjectContext ShellCommand
    typeCheckApp = return "npx tsc --build tsconfig.src.json"

    writeQuery :: T.Text -> ShellCommandBuilder WaspProjectContext ShellCommand
    writeQuery result = do
      context <- ask
      writeToFile
        (context.waspProjectDir </> [relfile|src/queries.ts|])
        [trimming|
          import type { GetTasks } from "wasp/server/operations";

          export const getTasks = (async () => {
            return $result;
          }) satisfies GetTasks<void>;
        |]

    writeMainPage :: ShellCommandBuilder WaspProjectContext ShellCommand
    writeMainPage = do
      context <- ask
      writeToFile
        (context.waspProjectDir </> [relfile|src/MainPage.tsx|])
        [trimming|
          import { getTasks, useQuery } from "wasp/client/operations";

          export function MainPage() {
            const { data: tasks } = useQuery(getTasks);
            return <div>{tasks?.map((task) => <p key={task.id}>{task.id}</p>)}</div>;
          }
        |]

    mainWaspTs :: T.Text
    mainWaspTs =
      [trimming|
        import { app, page, query, route } from "@wasp.sh/spec";
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

    textWaspVersion :: T.Text
    textWaspVersion = T.pack . show $ waspVersion
