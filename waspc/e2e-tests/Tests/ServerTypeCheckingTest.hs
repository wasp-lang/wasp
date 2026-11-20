module Tests.ServerTypeCheckingTest (serverTypeCheckingTest) where

import qualified Data.Text as T
import NeatInterpolation (trimming)
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    WaspProjectContext,
    appendToPrismaFile,
    createTestWaspProject,
    inTestWaspProjectDir,
    replaceMainWaspTsFile,
    waspCliCompile,
  )
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Version (waspVersion)

-- | Type checks the generated server for app configurations that
-- kitchen-sink doesn't cover.
serverTypeCheckingTest :: Test
serverTypeCheckingTest =
  Test
    "server-type-checking"
    [ TestCase
        "type-checks-crud-without-auth"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ appendToPrismaFile prismaTaskModel,
                  replaceMainWaspTsFile crudWithoutAuthMainWaspTs,
                  waspCliCompile,
                  typeCheckServer
                ]
            ]
        )
    ]
  where
    typeCheckServer :: ShellCommandBuilder WaspProjectContext ShellCommand
    typeCheckServer = return "(cd .wasp/out/server && npx tsc --build)"

    prismaTaskModel :: T.Text
    prismaTaskModel =
      [trimming|
        model Task {
          id          Int    @id @default(autoincrement())
          description String
        }
      |]

    crudWithoutAuthMainWaspTs :: T.Text
    crudWithoutAuthMainWaspTs =
      [trimming|
        import { app, crud, page, route } from "@wasp/spec";
        import { MainPage } from "./src/MainPage" with { type: "ref" };

        export default app({
          name: "crudWithoutAuth",
          title: "CRUD without auth",
          wasp: { version: "$textWaspVersion" },
          spec: [
            route("RootRoute", "/", page(MainPage)),
            crud("tasks", "Task", {
              get: {},
              getAll: {},
              create: {},
              update: {},
              delete: { isPublic: true },
            }),
          ],
        });
      |]

    textWaspVersion :: T.Text
    textWaspVersion = T.pack . show $ waspVersion
