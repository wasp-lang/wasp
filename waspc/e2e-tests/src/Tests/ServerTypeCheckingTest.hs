module Tests.ServerTypeCheckingTest (test_serverTypeChecking) where

import qualified Data.Text as T
import E2E
import NeatInterpolation (trimming)
import qualified StrongPath as SP
import Tests.Fixtures (taskPrismaModel)
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Generator.ServerGenerator.Common (serverRootDirInGeneratedAppDir)
import Wasp.Project.Common (generatedAppDirInWaspProjectDir)

-- | Type checks the generated server for app configurations that
-- kitchen-sink doesn't cover.
test_serverTypeChecking :: TestTree
test_serverTypeChecking =
  testGroup
    "server-type-checking"
    [ test "type-checks-server-features-without-auth" $
        withNewWaspProject minimalStarterTemplate $ do
          setDbToPostgres -- Jobs need PostgreSQL.
          appendToFile prismaSchemaFile taskPrismaModel
          writeToFile "src/serverFeatures.ts" serverFeaturesTs
          writeToFile mainWaspTsFile serverFeaturesWithoutAuthMainWaspTs
          wasp ["compile"]
          run $ nodeModulesBin "tsc" ["--build", SP.fromRelDir $ generatedAppDirInWaspProjectDir SP.</> serverRootDirInGeneratedAppDir]
    ]
  where
    serverFeaturesTs :: T.Text
    serverFeaturesTs =
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
          wasp: { version: "$textWaspVersion" },
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
