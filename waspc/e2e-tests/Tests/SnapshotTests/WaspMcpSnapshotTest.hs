module Tests.SnapshotTests.WaspMcpSnapshotTest (waspMcpSnapshotTest) where

import qualified Data.Text as T
import NeatInterpolation (trimming)
import ShellCommands
  ( appendToPrismaFile,
    createFileInWaspProjectDir,
    createSnapshotWaspProjectFromMinimalStarter,
    inSnapshotWaspProjectDir,
    replaceMainWaspTsFile,
    waspCliCompile,
  )
import SnapshotTest (SnapshotTest, makeSnapshotTest)
import StrongPath (relfile)

-- | An app with the experimental MCP feature: `app.mcp` with scopes,
-- initialScopes, confirmations and a rate limit; tools from `expose.mcp` on
-- queries and actions (one destructive, with `describe`), and an MCP-only
-- query (`expose.client: false`) using `dashboardUrl`. Wasp adds the MCP
-- models to the Prisma schema.
waspMcpSnapshotTest :: SnapshotTest
waspMcpSnapshotTest =
  makeSnapshotTest
    "wasp-mcp"
    [ createSnapshotWaspProjectFromMinimalStarter,
      inSnapshotWaspProjectDir
        [ replaceMainWaspTsFile mainWaspTs,
          appendToPrismaFile prismaModels,
          createFileInWaspProjectDir [relfile|src/tasks/schemas.ts|] tasksSchemasFile,
          createFileInWaspProjectDir [relfile|src/tasks/operations.ts|] tasksOperationsFile,
          createFileInWaspProjectDir [relfile|src/tasks/describe.ts|] tasksDescribeFile,
          createFileInWaspProjectDir [relfile|src/mcp/ConnectPage.tsx|] connectPageFile,
          createFileInWaspProjectDir [relfile|src/mcp/ConnectionsPage.tsx|] connectionsPageFile,
          createFileInWaspProjectDir [relfile|src/mcp/ConfirmPage.tsx|] confirmPageFile,
          createFileInWaspProjectDir [relfile|src/auth/AuthPages.tsx|] authPagesFile,
          waspCliCompile
        ]
    ]
  where
    mainWaspTs :: T.Text
    mainWaspTs =
      [trimming|
        import { action, app, page, query, route } from "@wasp/spec";
        import { ConnectPage } from "./src/mcp/ConnectPage" with { type: "ref" };
        import { ConnectionsPage } from "./src/mcp/ConnectionsPage" with { type: "ref" };
        import { ConfirmPage } from "./src/mcp/ConfirmPage" with { type: "ref" };
        import { LoginPage, SignupPage } from "./src/auth/AuthPages" with { type: "ref" };
        import { MainPage } from "./src/MainPage" with { type: "ref" };
        import { describeDeleteTask } from "./src/tasks/describe" with { type: "ref" };
        import {
          createTask,
          deleteTask,
          getTasks,
          getTaskStats,
        } from "./src/tasks/operations" with { type: "ref" };
        import {
          deleteTaskInput,
          createTaskInput,
          getTasksInput,
          taskOut,
          tasksOut,
        } from "./src/tasks/schemas" with { type: "ref" };

        export default app({
          name: "waspApp",
          wasp: { version: "^0.27.0" },
          title: "wasp-app",
          head: ["<link rel='icon' href='/favicon.ico' />"],
          auth: {
            userEntity: "User",
            methods: { usernameAndPassword: {} },
            onAuthFailedRedirectTo: "/login",
          },
          mcp: {
            instructions:
              "Tasks keeps the user's to-do list. Start with list_tasks; use create_task to add one.",
            consentRoute: "McpConsentRoute",
            confirmRoute: "McpConfirmRoute",
            scopes: {
              "tasks:read": { label: "View your tasks" },
              "tasks:write": {
                label: "Add and change your tasks",
                description: "Including marking them done.",
                implies: ["tasks:read"],
              },
            },
            // Clients start read-only and ask for tasks:write when they need it.
            initialScopes: ["tasks:read"],
            rateLimit: { max: 50 },
          },
          spec: [
            route("RootRoute", "/", page(MainPage)),
            route("TaskRoute", "/tasks/:id", page(MainPage)),
            route("LoginRoute", "/login", page(LoginPage)),
            route("SignupRoute", "/signup", page(SignupPage)),
            route(
              "McpConsentRoute",
              "/connect",
              page(ConnectPage, { authRequired: true }),
            ),
            route(
              "McpConfirmRoute",
              "/confirm",
              page(ConfirmPage, { authRequired: true }),
            ),
            action(deleteTask, {
              entities: ["Task"],
              expose: {
                mcp: {
                  description: "Deletes one of the user's tasks for good.",
                  input: deleteTaskInput,
                  scope: "tasks:write",
                  destructive: { describe: describeDeleteTask },
                },
              },
            }),
            route(
              "ConnectionsRoute",
              "/connections",
              page(ConnectionsPage, { authRequired: true }),
            ),
            query(getTasks, {
              entities: ["Task"],
              expose: {
                mcp: {
                  // Overrides the default name, get_tasks.
                  name: "list_tasks",
                  scope: "tasks:read",
                  title: "List tasks",
                  description:
                    "Lists the user's tasks, newest first. Pass `done` to filter by status.",
                  input: getTasksInput,
                  output: tasksOut,
                },
              },
            }),
            action(createTask, {
              entities: ["Task"],
              expose: {
                mcp: {
                  scope: "tasks:write",
                  description: "Adds a task to the user's list and returns it.",
                  input: createTaskInput,
                  output: taskOut,
                },
              },
            }),
            query(getTaskStats, {
              entities: ["Task"],
              expose: {
                client: false,
                mcp: {
                  description: "Counts the user's open and done tasks.",
                  input: getTasksInput,
                  idempotent: true,
                },
              },
            }),
          ],
        });
      |]

    prismaModels :: T.Text
    prismaModels =
      [trimming|
        model User {
          id             Int             @id @default(autoincrement())
          tasks          Task[]
        }

        model Task {
          id          Int      @id @default(autoincrement())
          description String
          isDone      Boolean  @default(false)
          createdAt   DateTime @default(now())
          user        User     @relation(fields: [userId], references: [id], onDelete: Cascade)
          userId      Int
        }
      |]

    tasksSchemasFile :: T.Text
    tasksSchemasFile =
      [trimming|
        import { z } from "zod";

        export const getTasksInput = z.object({
          done: z.boolean().optional().describe("Only tasks with this status."),
        });
        export type GetTasksInput = z.infer<typeof getTasksInput>;

        export const createTaskInput = z.object({
          description: z.string().min(1).max(200).describe("What needs doing."),
        });
        export type CreateTaskInput = z.infer<typeof createTaskInput>;

        export const taskOut = z.object({
          id: z.number(),
          description: z.string(),
          isDone: z.boolean(),
          createdAt: z.iso.datetime(),
        });
        export type TaskOut = z.infer<typeof taskOut>;

        export const tasksOut = z.object({ tasks: z.array(taskOut) });
        export type TasksOut = z.infer<typeof tasksOut>;

        export const deleteTaskInput = z.object({ id: z.number().describe("The task's ID.") });
        export type DeleteTaskInput = z.infer<typeof deleteTaskInput>;
      |]

    tasksOperationsFile :: T.Text
    tasksOperationsFile =
      [trimming|
        import { HttpError } from "wasp/server";
        import { dashboardUrl } from "wasp/server/mcp";
        import type {
          CreateTask,
          DeleteTask,
          GetTasks,
          GetTaskStats,
        } from "wasp/server/operations";
        import type {
          CreateTaskInput,
          DeleteTaskInput,
          GetTasksInput,
          TaskOut,
          TasksOut,
        } from "./schemas";

        const toView = (task: {
          id: number;
          description: string;
          isDone: boolean;
          createdAt: Date;
        }): TaskOut => ({
          id: task.id,
          description: task.description,
          isDone: task.isDone,
          createdAt: task.createdAt.toISOString(),
        });

        export const getTasks: GetTasks<GetTasksInput, TasksOut> = async (
          args,
          context,
        ) => {
          if (!context.user) throw new HttpError(401);
          const tasks = await context.entities.Task.findMany({
            where: { userId: context.user.id, isDone: args?.done },
            orderBy: { createdAt: "desc" },
          });
          return { tasks: tasks.map(toView) };
        };

        export const createTask: CreateTask<CreateTaskInput, TaskOut> = async (
          args,
          context,
        ) => {
          if (!context.user) throw new HttpError(401);
          // context.mcp is set when an MCP client calls this as a tool.
          const via = context.mcp ? " (via " + context.mcp.connection.clientName + ")" : "";
          const task = await context.entities.Task.create({
            data: { description: args.description + via, userId: context.user.id },
          });
          return toView(task);
        };

        export const getTaskStats: GetTaskStats<
          GetTasksInput,
          { open: number; done: number; dashboardUrl: string; newestTaskUrl: string | null }
        > = async (_args, context) => {
          if (!context.user) throw new HttpError(401);
          const where = { userId: context.user.id };
          const newest = await context.entities.Task.findFirst({
            where,
            orderBy: { createdAt: "desc" },
          });
          return {
            open: await context.entities.Task.count({ where: { ...where, isDone: false } }),
            done: await context.entities.Task.count({ where: { ...where, isDone: true } }),
            // Links back into the app, built from route names.
            dashboardUrl: dashboardUrl("RootRoute", { search: { view: "stats" } }),
            newestTaskUrl: newest
              ? dashboardUrl("TaskRoute", { params: { id: newest.id } })
              : null,
          };
        };

        export const deleteTask: DeleteTask<DeleteTaskInput, { id: number }> = async (
          args,
          context,
        ) => {
          if (!context.user) throw new HttpError(401);
          const { count } = await context.entities.Task.deleteMany({
            where: { id: args.id, userId: context.user.id },
          });
          if (count === 0)
            throw new HttpError(404, "Task " + args.id + " not found.", {
              nextStep: "Use list_tasks to find valid IDs.",
            });
          return { id: args.id };
        };
      |]

    tasksDescribeFile :: T.Text
    tasksDescribeFile =
      [trimming|
        import { prisma } from "wasp/server";
        import type { McpDescribeFn } from "wasp/server/mcp";
        import type { DeleteTaskInput } from "./schemas";

        /** What the confirm page shows for delete_task. */
        export const describeDeleteTask: McpDescribeFn<DeleteTaskInput> = async (
          { id },
          { user },
        ) => {
          const task = await prisma.task.findFirst({
            where: { id, userId: user.id },
          });
          return {
            title: task
              ? "Delete the task \"" + task.description + "\""
              : "Delete task " + id,
            details: "This can't be undone.",
          };
        };
      |]

    connectPageFile :: T.Text
    connectPageFile =
      [trimming|
        import { McpConsentForm } from "wasp/client/mcp";

        export function ConnectPage() {
          return <McpConsentForm appName="wasp-app" />;
        }
      |]

    connectionsPageFile :: T.Text
    connectionsPageFile =
      [trimming|
        import { McpConnections } from "wasp/client/mcp";

        export function ConnectionsPage() {
          return <McpConnections />;
        }
      |]

    confirmPageFile :: T.Text
    confirmPageFile =
      [trimming|
        import { McpConfirmAction } from "wasp/client/mcp";

        export function ConfirmPage() {
          return <McpConfirmAction />;
        }
      |]

    authPagesFile :: T.Text
    authPagesFile =
      [trimming|
        import { LoginForm, SignupForm } from "wasp/client/auth";

        export function LoginPage() {
          return <LoginForm />;
        }

        export function SignupPage() {
          return <SignupForm />;
        }
      |]
