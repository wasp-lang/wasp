// The config, resolved once: defaults filled in, URLs built, tools indexed.
// Every module takes a Runtime instead of the raw config.
import { z } from "zod";
import { expandScopes } from "../scopes";
import type { AnyMcpTool, McpServerConfig } from "./config";
import { deriveKey } from "./crypto";
import type { ConnectionRecord, ConnectionStore } from "./store";

/**
 * Connections made before the app declared any scopes (one all-or-nothing
 * consent) have no requested scopes. They count as granted every declared
 * scope, so adding scopes to an app doesn't lock out connected clients.
 * Every connection made with scopes has requested at least one.
 */
function withLegacyScopes(
  store: ConnectionStore,
  allScopes: string[],
): ConnectionStore {
  const normalize = <T extends ConnectionRecord | null>(c: T): T =>
    c && c.requestedScopes.length === 0 && allScopes.length > 0
      ? { ...c, scopes: allScopes, requestedScopes: allScopes }
      : c;
  return {
    ...store,
    getConnection: async (id) => normalize(await store.getConnection(id)),
    findActiveConnection: async (userId, clientId) =>
      normalize(await store.findActiveConnection(userId, clientId)),
    listActiveConnections: async (userId) =>
      (await store.listActiveConnections(userId)).map(normalize),
  };
}

export const OAUTH_PATHS = {
  token: "/mcp/oauth/token",
  revoke: "/mcp/oauth/revoke",
  protectedResource: "/.well-known/oauth-protected-resource",
  authorizationServer: "/.well-known/oauth-authorization-server",
} as const;

// `any`: internal modules never look at the user, and `User` appears in handler parameters.
export type Runtime<User = any> = ReturnType<typeof createRuntime<User>>;

const stripSlash = (url: string) => url.replace(/\/+$/, "");

/**
 * Tool arguments and results are plain JSON, so their schemas must be too
 * (D24): `z.iso.datetime()` instead of `z.date()`, no bigint, Map, or output
 * transforms. `z.toJSONSchema` throws on those, so checking at startup turns a
 * broken `tools/list` into an error that names the tool.
 */
function assertJsonSchemas(tool: AnyMcpTool<any>) {
  // Not `instanceof`: the app and the lib may resolve different copies of zod.
  if (tool.input._zod.def.type !== "object") {
    throw new Error(
      `MCP tool "${tool.name}": input must be a z.object(), since MCP arguments are always an object.`,
    );
  }
  const schemas = [
    ["input", tool.input],
    ["output", tool.output],
  ] as const;
  for (const [io, schema] of schemas) {
    if (!schema) continue;
    try {
      z.toJSONSchema(schema, { io });
    } catch (e) {
      const reason = e instanceof Error ? e.message : String(e);
      throw new Error(
        `MCP tool "${tool.name}": its ${io} schema can't be sent as JSON (${reason}). Use JSON types, e.g. z.iso.datetime() instead of z.date().`,
      );
    }
  }
}

/**
 * The config is generated from a Wasp app that `wasp compile` validated
 * (Valid/Mcp.hs: tool names, scopes, confirm route; env.ts: the secret), so
 * only what needs the runtime's zod is checked here.
 */
export function createRuntime<User>(config: McpServerConfig<User>) {
  const tools = new Map<string, AnyMcpTool<User>>();
  for (const tool of config.tools) {
    assertJsonSchemas(tool);
    tools.set(tool.name, tool);
  }

  const serverUrl = stripSlash(config.urls.serverUrl);
  const frontendUrl = stripSlash(config.urls.frontendUrl);
  const mcpPath = config.urls.mcpPath ?? "/mcp";

  return {
    config,
    connections: withLegacyScopes(
      config.store.connections,
      Object.keys(config.scopes),
    ),
    pendingActions: config.store.pendingActions,
    keys: {
      access: deriveKey(config.secret, "access token"),
      refresh: deriveKey(config.secret, "refresh token"),
      code: deriveKey(config.secret, "authorization code"),
      requestState: deriveKey(config.secret, "request state").toString(
        "base64url",
      ),
    },
    /** CIMD documents by client ID. Only read at consent and token exchange, so memory is enough. */
    clientCache: new Map<string, { metadata: unknown; fetchedAt: Date }>(),
    scopes: config.scopes,
    tools,
    now: config.now ?? (() => new Date()),
    insufficientScope: config.insufficientScope ?? "stepUp",
    initialScopes: expandScopes(
      config.scopes,
      config.initialScopes ?? Object.keys(config.scopes),
    ),
    urls: {
      issuer: serverUrl,
      frontendUrl,
      mcpPath,
      mcpResource: `${serverUrl}${mcpPath}`,
      protectedResourceMetadata: `${serverUrl}${OAUTH_PATHS.protectedResource}${mcpPath}`,
      authorizationEndpoint: `${frontendUrl}${config.urls.consentPath}`,
      tokenEndpoint: `${serverUrl}${OAUTH_PATHS.token}`,
      revocationEndpoint: `${serverUrl}${OAUTH_PATHS.revoke}`,
      confirmUrl: (actionId: string) =>
        `${frontendUrl}${config.urls.confirmPath!(actionId)}`,
    },
  };
}
