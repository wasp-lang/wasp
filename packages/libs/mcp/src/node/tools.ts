// Running tools, shared by direct calls and by approvals on the confirm page,
// plus the built-in whoami tool and the result envelope destructive tools use.
import { z } from "zod";
import { expandScopes } from "../scopes";
import type { AnyMcpTool, McpOperationContext, McpPrincipal } from "./config";
import type { Runtime } from "./runtime";

const WHOAMI_TOOL_NAME = "whoami";

export const isDestructive = (tool: AnyMcpTool<any>): boolean =>
  Boolean(tool.destructive);

/** The context a tool's operation gets: the connection's user, and `mcp` describing the connection. */
export async function operationContext<User>(
  rt: Runtime<User>,
  principal: McpPrincipal,
): Promise<McpOperationContext<User>> {
  return {
    user: await rt.config.getUser(principal.userId),
    mcp: {
      scopes: expandScopes(rt.scopes, principal.scopes),
      connection: {
        id: principal.connectionId,
        clientId: principal.clientId,
        clientName: principal.clientName,
        requireConfirmation: principal.requireConfirmation,
      },
    },
  };
}

/**
 * Validates the args and calls the tool's operation. This check stands in for
 * the one the SDK wrapper will do (D21), and runs on approvals too, where the
 * MCP SDK doesn't. The result isn't parsed: Wasp never validates output (D22).
 */
export async function runTool<User>(
  rt: Runtime<User>,
  tool: AnyMcpTool<User>,
  rawArgs: unknown,
  principal: McpPrincipal,
): Promise<unknown> {
  const args = tool.input.parse(rawArgs ?? {});
  return tool.operation(args, await operationContext(rt, principal));
}

export const hasScope = (
  rt: Runtime<any>,
  principal: McpPrincipal,
  scope: string | undefined,
): boolean =>
  !scope || expandScopes(rt.scopes, principal.scopes).includes(scope);

/** The scopes to ask for in a step-up: what's granted plus what's missing, so re-consent keeps the other tools. */
export function stepUpScopes(
  rt: Runtime<any>,
  principal: McpPrincipal,
  scope: string,
): [string, ...string[]] {
  const scopes = expandScopes(rt.scopes, [...principal.scopes, scope]);
  return scopes as [string, ...string[]];
}

const ConfirmationStatus = z
  .enum(["completed", "confirmation_required", "denied", "expired"])
  .describe(
    "completed: the tool ran; its output is in `result`. confirmation_required: nothing happened yet; share `confirmUrl` with the user, the tool runs once they approve. denied: the user said no. expired: the user didn't decide in time.",
  );

/** Destructive tools always answer in this envelope, whether or not the user had to approve. */
export function destructiveOutput(output: z.ZodType | undefined) {
  return z.object({
    status: ConfirmationStatus,
    result: (output ?? z.unknown())
      .optional()
      .describe("The tool's output (status completed)."),
    confirmUrl: z
      .string()
      .optional()
      .describe(
        "The page where the user approves or denies (status confirmation_required).",
      ),
    actionId: z.string().optional().describe("The pending action's ID."),
    expiresAt: z
      .string()
      .optional()
      .describe("When the confirmation expires (ISO 8601)."),
    message: z.string().optional().describe("What to tell the user."),
  });
}

export const DESTRUCTIVE_DESCRIPTION_SUFFIX =
  " This changes something that can't be undone, so the user may have to approve it in the app first: clients that can open web pages show the approval page themselves, otherwise the result has status confirmation_required and a confirmUrl to share with the user. Nothing happens until they approve.";

export function whoamiTool<User>(rt: Runtime<User>): AnyMcpTool<User> {
  const describeUser = rt.config.describeUser;
  return {
    name: WHOAMI_TOOL_NAME,
    title: "Who am I",
    description: `Returns the ${rt.config.title ?? rt.config.name} account this connection acts for, the permissions (scopes) it was granted, and whether destructive actions need the user's approval. Call it first if you're unsure what you can do.`,
    input: z.object({}),
    output: z.object({
      user: z.record(z.string(), z.unknown()),
      scopes: z.array(z.string()),
      requireConfirmation: z.boolean(),
      client: z.object({ id: z.string(), name: z.string() }),
    }),
    readOnly: true,
    idempotent: true,
    operation: async (_args, { user, mcp }) => ({
      user: describeUser
        ? describeUser(user)
        : { id: (user as { id?: unknown } | null)?.id ?? null },
      scopes: mcp.scopes,
      requireConfirmation: mcp.connection.requireConfirmation,
      client: { id: mcp.connection.clientId, name: mcp.connection.clientName },
    }),
  };
}
