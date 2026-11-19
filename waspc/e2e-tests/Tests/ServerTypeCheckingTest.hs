module Tests.ServerTypeCheckingTest (serverTypeCheckingTest) where

import Control.Monad.Reader (ask)
import qualified Data.Text as T
import NeatInterpolation (trimming)
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    WaspProjectContext (..),
    appendToPrismaFile,
    createTestWaspProject,
    inTestWaspProjectDir,
    replaceMainWaspTsFile,
    setWaspDbToPSQL,
    waspCliCompile,
    writeToFile,
  )
import StrongPath (relfile, (</>))
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

-- | Type checks the generated server for app configurations that
-- kitchen-sink doesn't cover.
serverTypeCheckingTest :: Test
serverTypeCheckingTest =
  Test
    "server-type-checking"
    [ TestCase
        "type-checks-server-features-without-auth"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ setWaspDbToPSQL, -- Jobs need PostgreSQL.
                  appendToPrismaFile prismaTaskModel,
                  writeServerFeaturesFile,
                  replaceMainWaspTsFile serverFeaturesWithoutAuthMainWaspTs,
                  installFeaturePackages,
                  waspCliCompile,
                  typeCheckServer
                ]
            ]
        )
    ]
  where
    -- Jobs and WebSockets need their packages in the project.
    installFeaturePackages :: ShellCommandBuilder WaspProjectContext ShellCommand
    installFeaturePackages =
      return
        "npm install \"pg-boss@^8.4.2\" \"socket.io@^4.6.1\" \"socket.io-client@^4.6.1\" \"@socket.io/component-emitter@^4.0.0\""

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

    writeServerFeaturesFile :: ShellCommandBuilder WaspProjectContext ShellCommand
    writeServerFeaturesFile = do
      context <- ask
      writeToFile
        (context.waspProjectDir </> [relfile|src/serverFeatures.ts|])
        [trimming|
          import type { DbSeedFn, MiddlewareConfigFn, ServerSetupFn } from "wasp/server";
          import type { GetTasksApi } from "wasp/server/api";
          import type { CleanUpTasks } from "wasp/server/jobs";
          import type { CreateTask, GetTasks } from "wasp/server/operations";
          import type { WebSocketDefinition } from "wasp/server/webSocket";

          export const getTasks: GetTasks = (_args, context) => context.entities.Task.findMany();

          export const createTask: CreateTask<{ description: string }> = (args, context) =>
            context.entities.Task.create({ data: args });

          export const getTasksApi: GetTasksApi = (_req, res) => {
            res.json([]);
          };

          export const cleanUpTasks: CleanUpTasks<{}, void> = async (_args, context) => {
            await context.entities.Task.deleteMany();
          };

          export const webSocketFn: WebSocketDefinition = (io) => {
            io.on("connection", () => {});
          };

          export const seedTasks: DbSeedFn = async (prisma) => {
            await prisma.task.deleteMany();
          };

          export const serverSetup: ServerSetupFn = async () => {};

          export const middlewareConfigFn: MiddlewareConfigFn = (middlewareConfig) => middlewareConfig;
        |]

    serverFeaturesWithoutAuthMainWaspTs :: T.Text
    serverFeaturesWithoutAuthMainWaspTs =
      [trimming|
        import { action, api, apiNamespace, app, crud, job, page, query, route } from "@wasp/spec";
        import { MainPage } from "./src/MainPage" with { type: "ref" };
        import {
          cleanUpTasks,
          createTask,
          getTasks,
          getTasksApi,
          middlewareConfigFn,
          seedTasks,
          serverSetup,
          webSocketFn,
        } from "./src/serverFeatures" with { type: "ref" };

        export default app({
          name: "serverFeaturesWithoutAuth",
          title: "Server features without auth",
          server: { setupFn: serverSetup, middlewareConfigFn },
          webSocket: { fn: webSocketFn },
          db: { seeds: [seedTasks] },
          spec: [
            route("RootRoute", "/", page(MainPage)),
            query(getTasks, { entities: ["Task"] }),
            action(createTask, { entities: ["Task"] }),
            apiNamespace("/api", { middlewareConfigFn }),
            api("GET", "/api/tasks", getTasksApi, { entities: ["Task"], middlewareConfigFn }),
            job(cleanUpTasks, { executor: "PgBoss", entities: ["Task"] }),
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
