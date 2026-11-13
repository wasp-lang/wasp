// The app's MCP server, from `app.mcp` and the operations with `expose.mcp`.
import {
  createPrismaStore,
  createWaspMcp,
  defineMcpToolFor,
  type McpOperationContext,
} from '@wasp/lib-mcp/node'
import type { AuthUser } from 'wasp/auth'
import { createAuthUserData, makeAuthUserIfPossible } from 'wasp/auth/user'
import { config, env, prisma } from 'wasp/server'
import {
  getTasks,
  getTaskStats,
  deleteTask,
  createTask,
} from 'wasp/server/operations'
import { getTasksInput as _waspMcpInput_getTasks } from "../../../../../src/tasks/schemas"
import { tasksOut as _waspMcpOutput_getTasks } from "../../../../../src/tasks/schemas"
import { getTasksInput as _waspMcpInput_getTaskStats } from "../../../../../src/tasks/schemas"
import { deleteTaskInput as _waspMcpInput_deleteTask } from "../../../../../src/tasks/schemas"
import { describeDeleteTask as _waspMcpDescribe_deleteTask } from "../../../../../src/tasks/describe"
import { createTaskInput as _waspMcpInput_createTask } from "../../../../../src/tasks/schemas"
import { taskOut as _waspMcpOutput_createTask } from "../../../../../src/tasks/schemas"

const defineTool = defineMcpToolFor<AuthUser>()

/** The scopes users can grant a client (`app.mcp.scopes`), in declaration order. */
const mcpScopes = {
  "tasks:read": {"label":"View your tasks"},
  "tasks:write": {"description":"Including marking them done.","implies":["tasks:read"],"label":"Add and change your tasks"},
}

// An operation as a tool calls it. In the server build, `wasp/server/operations`
// only knows operations as `Payload` in and out (their own types come from the
// user's code), so the tool's schemas can't be checked against them here. The
// schemas move onto the operations in v1 (D4), which brings that check.
type ToolOperation = (
  args: unknown,
  context: McpOperationContext<AuthUser>,
) => Promise<any>

// Tools call their operation through an arrow instead of a direct reference,
// because `wasp/server/operations` and this module can import each other.
const tools = [
  defineTool({
    name: "list_tasks",
    title: "List tasks",
    scope: "tasks:read",
    description: "Lists the user's tasks, newest first. Pass `done` to filter by status.",
    input: _waspMcpInput_getTasks,
    output: _waspMcpOutput_getTasks,
    readOnly: true,
    idempotent: false,
    openWorld: false,
    // Destructive tools wait for the user's approval on the confirm page.
    destructive: false,
    operation: (args, context) =>
      (getTasks as unknown as ToolOperation)(args, context),
  }),
  defineTool({
    name: "get_task_stats",
    description: "Counts the user's open and done tasks.",
    input: _waspMcpInput_getTaskStats,
    readOnly: true,
    idempotent: true,
    openWorld: false,
    // Destructive tools wait for the user's approval on the confirm page.
    destructive: false,
    operation: (args, context) =>
      (getTaskStats as unknown as ToolOperation)(args, context),
  }),
  defineTool({
    name: "delete_task",
    scope: "tasks:write",
    description: "Deletes one of the user's tasks for good.",
    input: _waspMcpInput_deleteTask,
    readOnly: false,
    idempotent: false,
    openWorld: false,
    // Destructive tools wait for the user's approval on the confirm page.
    destructive: {
      // Writes what the confirm page shows.
      describe: (args, context) =>
        _waspMcpDescribe_deleteTask(args, context),
    },
    operation: (args, context) =>
      (deleteTask as unknown as ToolOperation)(args, context),
  }),
  defineTool({
    name: "create_task",
    scope: "tasks:write",
    description: "Adds a task to the user's list and returns it.",
    input: _waspMcpInput_createTask,
    output: _waspMcpOutput_createTask,
    readOnly: false,
    idempotent: false,
    openWorld: false,
    // Destructive tools wait for the user's approval on the confirm page.
    destructive: false,
    operation: (args, context) =>
      (createTask as unknown as ToolOperation)(args, context),
  }),
]

async function getUser(userId: string): Promise<AuthUser> {
  const user = await prisma.user.findUniqueOrThrow({
    where: { id: Number(userId) },
    include: { auth: { include: { identities: true } } },
  })
  return makeAuthUserIfPossible(createAuthUserData(user))
}

/** What the built-in whoami tool says about the user: their ID and how they log in. */
function describeUser(user: AuthUser): Record<string, unknown> {
  const identities = Object.fromEntries(
    Object.entries(user.identities)
      .filter(([, identity]) => identity)
      .map(([provider, identity]) => [provider, identity!.id]),
  )
  // JSON can't hold a BigInt id.
  const id = typeof user.id === 'bigint' ? String(user.id) : user.id
  return { id, identities }
}

export const mcp = createWaspMcp<AuthUser>({
  name: "waspApp",
  title: "wasp-app",
  instructions: "Tasks keeps the user's to-do list. Start with list_tasks; use create_task to add one.",
  scopes: mcpScopes,
  // What clients ask for when they first connect.
  initialScopes: ["tasks:read"],
  whoami: true,
  tools,
  urls: {
    serverUrl: config.serverUrl,
    frontendUrl: config.frontendUrl,
    consentPath: "/connect",
    confirmPath: (actionId) =>
      "/confirm" + '?action=' + encodeURIComponent(actionId),
  },
  // The McpConnection, McpPendingAction and McpRateLimit models Wasp adds
  // to the Prisma schema. Rate-limit counts are shared across server instances.
  store: createPrismaStore(prisma, {
    userIdToDb: (userId) => Number(userId),
  }),
  rateLimit: { windowMs: 60000, max: 50 },
  getUser,
  describeUser,
  secret: env.MCP_SECRET,
  allowLoopbackClients: config.isDevelopment,
})
