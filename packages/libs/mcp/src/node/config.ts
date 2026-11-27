import type { z } from "zod";
import type { ScopeDefinitions } from "../scopes";
import type { McpStore } from "./store";

export type InsufficientScopeBehavior = "stepUp" | "hide";

/** Who is calling a tool: the user, through one connected app. */
export type McpPrincipal = {
  userId: string;
  connectionId: string;
  clientId: string;
  clientName: string;
  scopes: string[];
  requireConfirmation: boolean;
};

/**
 * What an operation sees as `context.mcp` when it runs as a tool. Web calls
 * don't have it, so `context.mcp` being set means an agent is calling.
 */
export type McpCallContext = {
  /** Granted to this connection, with implied scopes included. */
  scopes: string[];
  connection: {
    id: string;
    /** The client's CIMD URL: its verified identity. */
    clientId: string;
    clientName: string;
    /** Whether destructive tools wait for the user's approval on this connection. */
    requireConfirmation: boolean;
  };
};

/** The context a tool's operation is called with: the connection's user, plus `mcp`. */
export type McpOperationContext<User = unknown> = {
  user: User;
  mcp: McpCallContext;
};

/**
 * A Wasp operation as the tool runtime calls it. Server-side operations from
 * `wasp/server/operations` fit this shape: they spread the context they get,
 * so the handler sees `context.mcp` next to `context.entities`.
 */
export type McpOperation<Args = never, Result = unknown, User = unknown> = (
  args: Args,
  context: McpOperationContext<User>,
) => Promise<Result>;

export type McpDescribeResult = { title: string; details?: string };

export type AnyZodObject = z.ZodObject<z.ZodRawShape>;

export type McpToolDefinition<
  Input extends AnyZodObject = AnyZodObject,
  Output extends z.ZodType | undefined = z.ZodType | undefined,
  User = unknown,
> = {
  /** What the model calls it: snake_case, verb_noun. */
  name: string;
  title?: string;
  /** Written for the model: when to use it instead of a related tool, what it returns, what to do next. */
  description: string;
  input: Input;
  output?: Output;
  /** The scope a connection needs to call it. Omit for tools any connection can call. */
  scope?: string;
  /** Overrides the server's `insufficientScope` for this tool. */
  insufficientScope?: InsufficientScopeBehavior;
  readOnly?: boolean;
  idempotent?: boolean;
  openWorld?: boolean;
  /**
   * Marks the tool `destructiveHint` without the confirm flow: clients may ask
   * the user first, but the server doesn't make it wait. Ignored when
   * `destructive` is set.
   */
  destructiveHint?: boolean;
  /**
   * Destructive tools wait for the user to approve them in the app, unless the
   * connection turned confirmations off. `describe` writes what the confirm page shows.
   */
  destructive?:
    | boolean
    | {
        describe?: (
          args: z.infer<Input>,
          context: McpOperationContext<User>,
        ) => Promise<McpDescribeResult> | McpDescribeResult;
      };
  /**
   * The operation the tool runs, called with the validated args and
   * `{ user, mcp }`. Its result goes to the model as is: `output` describes it
   * in `tools/list` but doesn't filter it, so return only what the model
   * should see (D22).
   */
  operation: McpOperation<
    z.infer<Input>,
    Output extends z.ZodType ? z.infer<Output> : unknown,
    User
  >;
};

/** Erases a tool's generics so tools with different schemas fit in one list. */
export type AnyMcpTool<User = unknown> = McpToolDefinition<
  AnyZodObject,
  z.ZodType | undefined,
  User
>;

export function defineMcpTool<
  Input extends AnyZodObject,
  Output extends z.ZodType | undefined = undefined,
  User = unknown,
>(tool: McpToolDefinition<Input, Output, User>): AnyMcpTool<User> {
  return tool as unknown as AnyMcpTool<User>;
}

/**
 * `defineMcpTool` with the user type fixed, so operations and `describe` get a
 * typed `context.user` without annotating every tool:
 * `const defineTool = defineMcpToolFor<AuthUser>()`.
 */
export function defineMcpToolFor<User>() {
  return <
    Input extends AnyZodObject,
    Output extends z.ZodType | undefined = undefined,
  >(
    tool: McpToolDefinition<Input, Output, User>,
  ): AnyMcpTool<User> => defineMcpTool(tool);
}

export type McpServerConfig<User = unknown> = {
  name: string;
  title?: string;
  version?: string;
  /** Given to the model when it connects: what the app is and how the tools fit together. */
  instructions: string;
  scopes: ScopeDefinitions;
  /** What clients ask for when they first connect. Defaults to every scope. */
  initialScopes?: string[];
  /** What happens to tools whose scope wasn't granted. Defaults to "stepUp". */
  insufficientScope?: InsufficientScopeBehavior;
  /** Adds the built-in whoami tool. Defaults to true. */
  whoami?: boolean;
  tools: AnyMcpTool<User>[];

  urls: {
    /** Where the server runs, e.g. http://localhost:3001. The OAuth issuer and MCP resource live here. */
    serverUrl: string;
    /** Where the client runs, e.g. http://localhost:3000. The consent and confirm pages live here. */
    frontendUrl: string;
    /** The consent page's path. It is also the OAuth authorization endpoint. */
    consentPath: string;
    /** Builds the confirm page's path for a pending action. Required when a tool is destructive. */
    confirmPath?: (actionId: string) => string;
    /** Defaults to "/mcp". */
    mcpPath?: string;
  };

  store: McpStore;
  /** Loads the user a tool runs as. */
  getUser: (userId: string) => Promise<User>;
  /** What whoami says about the user. Defaults to `{ id }`. */
  describeUser?: (user: User) => Record<string, unknown>;
  /**
   * The server secret, at least 32 bytes and the same on every instance.
   * Seals auth codes, access tokens and refresh tokens, and signs the
   * `requestState` clients echo back. Changing it signs every client out
   * (their connections stay, so users only re-consent).
   */
  secret: string;
  /** Per-connection rate limit. Defaults to 120 requests per minute; false turns it off. */
  rateLimit?: { windowMs: number; max: number } | false;
  /** Allows http://localhost client IDs, for local development. Defaults to false. */
  allowLoopbackClients?: boolean;

  /** Hooks for tests and logging. */
  now?: () => Date;
  fetchClientMetadata?: (url: URL) => Promise<unknown>;
  log?: (entry: McpLogEntry) => void;
  confirmation?: {
    /**
     * Lets clients that support URL-mode elicitation open the confirm page
     * themselves. Off, every client gets the link (`confirmUrl`). Defaults to true.
     */
    elicitation?: boolean;
    waitMs?: number;
    pollMs?: number;
    maxWaits?: number;
  };
};

export type McpLogEntry = {
  tool: string;
  userId: string;
  connectionId: string;
  durationMs: number;
  outcome: "ok" | "error" | "confirmation_required";
};
