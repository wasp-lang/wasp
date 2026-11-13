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