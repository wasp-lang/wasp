module Tests.UserCodeTypeCheckingTest (userCodeTypeCheckingTest) where

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
    setWaspDbToPSQL,
    waspCliBuild,
    waspCliCompile,
    writeToFile,
  )
import StrongPath (relfile, (</>))
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Version (waspVersion)

userCodeTypeCheckingTest :: Test
userCodeTypeCheckingTest =
  Test
    "user-code-type-checking"
    [ TestCase
        "compile-rejects-server-error"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ replaceMainWaspTsFile mainWaspTsWithServerSetup,
                  writeServerSetupTs,
                  assertCommandOutputContains
                    (("! " ++) <$> waspCliCompile)
                    userCodeTypeCheckFailure
                ]
            ]
        ),
      TestCase
        "compile-rejects-unreferenced-file-error"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ writeTypeCheckTs "export const count: number = 'wrong'\n",
                  assertCommandOutputContains
                    (("! " ++) <$> waspCliCompile)
                    userCodeTypeCheckFailure
                ]
            ]
        ),
      TestCase
        "compile-updates-operation-types-and-recovers"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ replaceMainWaspTsFile mainWaspTsWithQuery,
                  writeQuery "[{ id: 1 }]",
                  writeMainPage,
                  waspCliCompile,
                  writeQuery "[]",
                  -- Editing a file makes TypeScript report the initial error.
                  appendToFile "src/MainPage.tsx" "",
                  assertCommandOutputContains
                    (("! " ++) <$> waspCliCompile)
                    "Property 'id' does not exist on type 'never'.",
                  writeQuery "[{ id: 1 }]",
                  waspCliCompile
                ]
            ]
        ),
      TestCase
        "build-rejects-user-code-error"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ setWaspDbToPSQL,
                  appendToFile "src/MainPage.tsx" "const shouldBeNumber: number = 'wrong'",
                  assertCommandOutputContains
                    (("! " ++) <$> waspCliBuild)
                    userCodeTypeCheckFailure
                ]
            ]
        )
    ]
  where
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

    mainWaspTsWithQuery :: T.Text
    mainWaspTsWithQuery =
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

    missingSideEffectImport :: T.Text
    missingSideEffectImport = "import './missing-side-effect-import'"

    userCodeTypeCheckFailure :: String
    userCodeTypeCheckFailure = "User code type-check failed with exit code:"

    mainWaspTsWithServerSetup :: T.Text
    mainWaspTsWithServerSetup =
      [trimming|
        import { app, page, route } from "@wasp.sh/spec"
        import { MainPage } from "./src/MainPage" with { type: "ref" }
        import { serverSetup } from "./src/serverSetup" with { type: "ref" }

        export default app({
          name: "typeCheckTest",
          title: "Type check test",
          wasp: { version: "$textWaspVersion" },
          server: { setupFn: serverSetup },
          spec: [route("RootRoute", "/", page(MainPage))]
        })
      |]

    writeServerSetupTs :: ShellCommandBuilder WaspProjectContext ShellCommand
    writeServerSetupTs = do
      context <- ask
      writeToFile
        (context.waspProjectDir </> [relfile|src/serverSetup.ts|])
        [trimming|
          $missingSideEffectImport

          export const serverSetup = () => {}
        |]

    writeTypeCheckTs :: T.Text -> ShellCommandBuilder WaspProjectContext ShellCommand
    writeTypeCheckTs contents = do
      context <- ask
      writeToFile (context.waspProjectDir </> [relfile|src/typeCheck.ts|]) contents
