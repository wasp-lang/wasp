// A small notes app on the MCP lib, driven in-process: OAuth through the real
// endpoints, and an MCP SDK client whose fetch goes straight to the handler.
import {
  Client,
  StreamableHTTPClientTransport,
  type ClientCapabilities,
  type ElicitRequest,
  type ElicitResult,
} from "@modelcontextprotocol/client";
import { randomBytes } from "node:crypto";
import { z } from "zod";
import {
  createMemoryStore,
  createWaspMcp,
  defineMcpTool,
  McpToolError,
  type McpServerConfig,
} from "../src/node";
import { pkceChallenge } from "../src/node/crypto";

export const SERVER_URL = "http://localhost:3001";
export const FRONTEND_URL = "http://localhost:3000";
export const CLIENT_ID = "https://client.example/oauth/metadata.json";
export const REDIRECT_URI = "http://127.0.0.1:9999/callback";

type User = { id: string; username: string };
type Note = { id: string; userId: string; text: string };

/** Mimics Wasp's HttpError: a status code and optional data. */
class HttpError extends Error {
  constructor(
    public statusCode: number,
    message: string,
    public data?: unknown,
  ) {
    super(message);
  }
}

export function createTestApp(overrides: Partial<McpServerConfig<User>> = {}) {
  const store = createMemoryStore();
  const users = new Map<string, User>([
    ["u1", { id: "u1", username: "alice" }],
    ["u2", { id: "u2", username: "bob" }],
  ]);
  const notes: Note[] = [];
  let nextId = 1;

  const tools = [
    defineMcpTool({
      name: "list_notes",
      title: "List notes",
      description: "Lists the user's notes.",
      input: z.object({}),
      output: z.object({
        notes: z.array(z.object({ id: z.string(), text: z.string() })),
      }),
      scope: "notes:read",
      readOnly: true,
      operation: async (_args, context: { user: User }) => ({
        notes: notes
          .filter((n) => n.userId === context.user.id)
          .map(({ id, text }) => ({ id, text })),
      }),
    }),
    defineMcpTool({
      name: "create_note",
      title: "Create a note",
      description: "Creates a note.",
      input: z.object({ text: z.string().min(1).max(200) }),
      output: z.object({ id: z.string(), text: z.string() }),
      scope: "notes:write",
      operation: async ({ text }, context: { user: User }) => {
        const note = { id: String(nextId++), userId: context.user.id, text };
        notes.push(note);
        return {
          id: note.id,
          text: note.text,
          // Not in the output schema, and still sent: output isn't filtered (D22).
          extra: "not in the output schema",
        };
      },
    }),
    defineMcpTool({
      name: "delete_note",
      title: "Delete a note",
      description: "Deletes a note.",
      input: z.object({ id: z.string() }),
      output: z.object({ id: z.string() }),
      scope: "notes:write",
      destructive: {
        describe: ({ id }) => ({
          title: `Delete note ${id}`,
          details: notes.find((n) => n.id === id)?.text,
        }),
      },
      operation: async ({ id }, context: { user: User }) => {
        const index = notes.findIndex(
          (n) => n.id === id && n.userId === context.user.id,
        );
        if (index === -1)
          throw new HttpError(404, `Note ${id} not found.`, {
            nextStep: "Use list_notes to find valid IDs.",
          });
        notes.splice(index, 1);
        return { id };
      },
    }),
    defineMcpTool({
      name: "explode",
      description: "Always fails.",
      input: z.object({ kind: z.enum(["tool", "internal"]) }),
      operation: async ({ kind }) => {
        if (kind === "tool")
          throw new McpToolError(
            "conflict",
            "Already exploded.",
            "Try something else.",
          );
        throw new Error("database password is hunter2");
      },
    }),
  ];

  const config: McpServerConfig<User> = {
    name: "notes",
    title: "Notes",
    instructions: "Notes keeps short notes. Use list_notes first.",
    scopes: {
      "notes:read": { label: "View your notes" },
      "notes:write": {
        label: "Create and delete notes",
        implies: ["notes:read"],
      },
    },
    tools: tools as McpServerConfig<User>["tools"],
    urls: {
      serverUrl: SERVER_URL,
      frontendUrl: FRONTEND_URL,
      consentPath: "/mcp/connect",
      confirmPath: (id) => `/mcp/confirm?action=${id}`,
    },
    store,
    getUser: async (id) => users.get(id)!,
    describeUser: (u) => ({ id: u.id, username: u.username }),
    secret: "test-secret-that-is-at-least-32-bytes-long",
    fetchClientMetadata: async (url) => ({
      client_id: url.toString(),
      client_name: "Test Client",
      redirect_uris: [REDIRECT_URI],
    }),
    log: () => {},
    confirmation: { waitMs: 300, pollMs: 10, maxWaits: 3 },
    ...overrides,
  };
  const mcp = createWaspMcp(config);

  /** Sends a request to the MCP routes, as an HTTP server would. */
  const send = async (path: string, init?: RequestInit) =>
    (await mcp.handle(new Request(`${SERVER_URL}${path}`, init))) ??
    new Response("not found", { status: 404 });

  /** Runs the OAuth flow as user `userId` approving `scopes` on the consent page. */
  async function authorize(
    userId: string,
    opts: {
      scopes?: string[];
      requested?: string[];
      requireConfirmation?: boolean;
    } = {},
  ) {
    const verifier = randomBytes(32).toString("base64url");
    const params = {
      response_type: "code",
      client_id: CLIENT_ID,
      redirect_uri: REDIRECT_URI,
      code_challenge: pkceChallenge(verifier),
      code_challenge_method: "S256",
      state: "xyz",
      scope: (opts.requested ?? opts.scopes ?? ["notes:write"]).join(" "),
      resource: `${SERVER_URL}/mcp`,
    };
    const { redirectTo } = await mcp.user.approveAuthorization(userId, {
      params,
      scopes: opts.scopes ?? ["notes:write"],
      requireConfirmation: opts.requireConfirmation ?? true,
    });
    const code = new URL(redirectTo).searchParams.get("code")!;
    const res = await send("/mcp/oauth/token", {
      method: "POST",
      headers: { "Content-Type": "application/x-www-form-urlencoded" },
      body: new URLSearchParams({
        grant_type: "authorization_code",
        code,
        redirect_uri: REDIRECT_URI,
        client_id: CLIENT_ID,
        code_verifier: verifier,
      }),
    });
    const tokens = (await res.json()) as {
      access_token: string;
      refresh_token: string;
      scope: string;
    };
    return { ...tokens, code, verifier, redirectTo };
  }

  type ConnectOptions = {
    capabilities?: ClientCapabilities;
    onElicit?: (
      params: ElicitRequest["params"],
    ) => ElicitResult | Promise<ElicitResult>;
    era?: "modern" | "legacy";
  };

  /** An MCP SDK client using `accessToken`. */
  async function connect(accessToken: string, opts: ConnectOptions = {}) {
    const transport = new StreamableHTTPClientTransport(
      new URL(`${SERVER_URL}/mcp`),
      {
        fetch: async (url, init) => {
          const request = new Request(url, init);
          request.headers.set("Authorization", `Bearer ${accessToken}`);
          return (
            (await mcp.handle(request)) ??
            new Response("not found", { status: 404 })
          );
        },
      },
    );
    const client = new Client(
      { name: "test", version: "1.0.0" },
      {
        capabilities: opts.capabilities ?? {},
        versionNegotiation: { mode: opts.era === "legacy" ? "legacy" : "auto" },
      },
    );
    if (opts.onElicit && opts.capabilities?.elicitation) {
      client.setRequestHandler("elicitation/create", (request) =>
        opts.onElicit!(request.params),
      );
    }
    await client.connect(transport);
    return {
      client,
      call: (name: string, args: Record<string, unknown> = {}) =>
        client.callTool({ name, arguments: args }),
      toolNames: async () =>
        (await client.listTools()).tools.map((t) => t.name).sort(),
      close: () => client.close(),
    };
  }

  /** A raw 2025-era JSON-RPC call, to see HTTP-level answers like 403 challenges. */
  const rawCall = (
    accessToken: string | null,
    name: string,
    args: Record<string, unknown> = {},
  ) =>
    send("/mcp", {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        Accept: "application/json, text/event-stream",
        "Mcp-Protocol-Version": "2025-11-25",
        ...(accessToken && { Authorization: `Bearer ${accessToken}` }),
      },
      body: JSON.stringify({
        jsonrpc: "2.0",
        id: 1,
        method: "tools/call",
        params: { name, arguments: args },
      }),
    });

  return { mcp, store, notes, users, send, authorize, connect, rawCall };
}

export type TestApp = ReturnType<typeof createTestApp>;
