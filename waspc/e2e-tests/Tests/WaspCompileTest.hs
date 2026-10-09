module Tests.WaspCompileTest (waspCompileTest) where

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

waspCompileTest :: Test
waspCompileTest =
  Test
    "wasp-compile"
    [ TestCase
        "fail-outside-project"
        (return [waspCliCompileFails]),
      TestCase
        "succeed-uncompiled-project"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ waspCliCompile,
                  return $ assertDirectoryExists ".wasp",
                  return $ assertDirectoryExists "node_modules"
                ]
            ]
        ),
      TestCase
        "succeed-compiled-project"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ waspCliCompile,
                  waspCliCompile,
                  return $ assertDirectoryExists ".wasp",
                  return $ assertDirectoryExists "node_modules"
                ]
            ]
        ),
      -- Regression test for https://github.com/wasp-lang/wasp/issues/2001
      TestCase
        "fail-missing-side-effect-import"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ appendToFile "src/MainPage.tsx" "import './missing-side-effect-import'",
                  assertCommandOutputContains
                    (("! " ++) <$> waspCliCompile)
                    "Cannot find module or type declarations for side-effect import of './missing-side-effect-import'."
                ]
            ]
        ),
      TestCase
        "fail-type-error"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ writeUtilsFile "export const shouldBeNumber: number = 'wrong'",
                  assertCommandOutputContains
                    (("! " ++) <$> waspCliCompile)
                    "src/utils.ts(1,14): error TS2322: Type 'string' is not assignable to type 'number'."
                ]
            ]
        ),
      -- Regression test for https://github.com/wasp-lang/wasp/pull/4942
      TestCase
        "fail-and-recover-with-updated-operation-types"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ replaceMainWaspTsFile mainWaspTsWithQuery,
                  writeQuery "[{ id: 1 }]",
                  writeMainPageUsingQuery,
                  waspCliCompile,
                  writeQuery "[]",
                  assertCommandOutputContains
                    (("! " ++) <$> waspCliCompile)
                    "Property 'id' does not exist on type 'never'.",
                  writeQuery "[{ id: 1 }]",
                  waspCliCompile
                ]
            ]
        )
    ]
  where
    waspCliCompileFails :: ShellCommand
    waspCliCompileFails = "! $WASP_CLI_CMD compile"

    assertDirectoryExists :: FilePath -> ShellCommand
    assertDirectoryExists dirFilePath = "[ -d '" ++ dirFilePath ++ "' ]"

    writeUtilsFile :: T.Text -> ShellCommandBuilder WaspProjectContext ShellCommand
    writeUtilsFile contents = do
      context <- ask
      writeToFile (context.waspProjectDir </> [relfile|src/utils.ts|]) contents

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

    writeMainPageUsingQuery :: ShellCommandBuilder WaspProjectContext ShellCommand
    writeMainPageUsingQuery = do
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
