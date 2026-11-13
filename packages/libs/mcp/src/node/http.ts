// Every HTTP route the MCP feature owns, as one web-standard handler:
// the MCP endpoint, OAuth metadata, and the token and revocation endpoints.
// The consent and confirm pages aren't here: they're app pages that call the
// user-facing functions.
import { readRequestBody } from "@modelcontextprotocol/server";
import { authenticate } from "./authenticate";
import type { McpPrincipal } from "./config";
import {
  authorizationServerMetadata,
  protectedResourceMetadata,
} from "./oauth/metadata";
import { exchangeToken, revokeToken } from "./oauth/token";
import { createMemoryRateLimitStore, createRateLimiter } from "./rateLimit";
import { OAUTH_PATHS, type Runtime } from "./runtime";
import { authInfoFor } from "./server";

type McpHttpHandler = {
  fetch: (request: Request, options?: { authInfo?: any }) => Promise<Response>;
};

/**
 * Browser-based MCP clients call these routes from any origin with bearer
 * tokens (never cookies), so every origin is allowed.
 */
const CORS_HEADERS: Record<string, string> = {
  "Access-Control-Allow-Origin": "*",
  "Access-Control-Allow-Methods": "GET, POST, DELETE, OPTIONS",
  "Access-Control-Allow-Headers":
    "Authorization, Content-Type, Accept, Mcp-Protocol-Version, Mcp-Session-Id, Mcp-Method, Mcp-Name, Last-Event-ID",
  "Access-Control-Expose-Headers":
    "WWW-Authenticate, Mcp-Protocol-Version, Mcp-Session-Id, Retry-After",
  "Access-Control-Max-Age": "86400",
};

function withCors(response: Response): Response {
  const headers = new Headers(response.headers);
  for (const [k, v] of Object.entries(CORS_HEADERS)) headers.set(k, v);
  return new Response(response.body, {
    status: response.status,
    statusText: response.statusText,
    headers,
  });
}

const json = (
  body: unknown,
  status = 200,
  headers: Record<string, string> = {},
) =>
  new Response(JSON.stringify(body), {
    status,
    headers: { "Content-Type": "application/json", ...headers },
  });

const jsonRpcError = (
  status: number,
  message: string,
  headers: Record<string, string> = {},
) =>
  json(
    { jsonrpc: "2.0", error: { code: -32000, message }, id: null },
    status,
    headers,
  );

/** OAuth request bodies are a few short parameters. */
const MAX_OAUTH_BODY_BYTES = 64 * 1024;

/** The token or revocation request's parameters; null if the body is too large. */
async function readParams(
  request: Request,
): Promise<Record<string, unknown> | null> {
  const body = await readRequestBody(request, MAX_OAUTH_BODY_BYTES);
  if (body.tooLarge) return null;
  const { text } = body;
  const type = request.headers.get("content-type") ?? "";
  if (type.includes("application/json")) {
    try {
      return JSON.parse(text);
    } catch {
      return {};
    }
  }
  return Object.fromEntries(new URLSearchParams(text));
}

const tooLarge = {
  status: 413,
  body: {
    error: "invalid_request",
    error_description: "The request body is too large.",
  },
} as const;

export function createHttpHandler(rt: Runtime<any>, mcp: McpHttpHandler) {
  const limit = rt.config.rateLimit ?? { windowMs: 60_000, max: 120 };
  const limiter =
    limit === false
      ? null
      : createRateLimiter(
          limit,
          rt.config.store.rateLimits ?? createMemoryRateLimitStore(),
        );
  const mcpPath = rt.urls.mcpPath;
  const protectedResourcePaths = new Set([
    OAUTH_PATHS.protectedResource,
    `${OAUTH_PATHS.protectedResource}${mcpPath}`,
  ]);

  // The body is read only after authentication, by the SDK, which caps its size.
  async function handleMcp(request: Request): Promise<Response> {
    if (request.method !== "POST") {
      // No sessions to resume or delete in stateless mode.
      return jsonRpcError(
        405,
        "Method not allowed: this server is stateless, use POST.",
        { Allow: "POST" },
      );
    }
    const principal: McpPrincipal | null = await authenticate(
      rt,
      request.headers.get("authorization"),
    );
    if (!principal) {
      return jsonRpcError(
        401,
        "Authorization required. Connect with OAuth: see the WWW-Authenticate header.",
        {
          // No scope parameter when the app declares none (RFC 6750 makes it optional).
          "WWW-Authenticate":
            `Bearer resource_metadata="${rt.urls.protectedResourceMetadata}"` +
            (rt.initialScopes.length
              ? `, scope="${rt.initialScopes.join(" ")}"`
              : ""),
        },
      );
    }
    const retryAfter = await limiter?.hit(principal.connectionId, rt.now());
    if (retryAfter) {
      return jsonRpcError(
        429,
        `Too many requests. Wait ${retryAfter} seconds before trying again.`,
        { "Retry-After": String(retryAfter) },
      );
    }
    return mcp.fetch(request, { authInfo: authInfoFor(rt, principal) });
  }

  /** Whether a path is one of the MCP routes. */
  function matches(pathname: string): boolean {
    return (
      pathname === mcpPath ||
      protectedResourcePaths.has(pathname) ||
      pathname === OAUTH_PATHS.authorizationServer ||
      pathname === OAUTH_PATHS.token ||
      pathname === OAUTH_PATHS.revoke
    );
  }

  /**
   * Handles a request if it's for one of the MCP routes, else returns null so
   * the app's own routes get it.
   */
  async function handle(request: Request): Promise<Response | null> {
    const { pathname } = new URL(request.url);
    if (!matches(pathname)) return null;
    if (request.method === "OPTIONS")
      return withCors(new Response(null, { status: 204 }));

    let response: Response;
    if (pathname === mcpPath) {
      response = await handleMcp(request);
    } else if (protectedResourcePaths.has(pathname)) {
      response = json(protectedResourceMetadata(rt), 200, {
        "Cache-Control": "public, max-age=300",
      });
    } else if (pathname === OAUTH_PATHS.authorizationServer) {
      response = json(authorizationServerMetadata(rt), 200, {
        "Cache-Control": "public, max-age=300",
      });
    } else if (request.method !== "POST") {
      response = json(
        { error: "invalid_request", error_description: "Use POST." },
        405,
        { Allow: "POST" },
      );
    } else if (pathname === OAUTH_PATHS.token) {
      try {
        const params = await readParams(request);
        const result = params ? await exchangeToken(rt, params) : tooLarge;
        response = json(result.body, result.status, {
          "Cache-Control": "no-store",
          Pragma: "no-cache",
        });
      } catch (e) {
        console.error(e);
        response = json(
          { error: "server_error", error_description: "Something went wrong." },
          500,
        );
      }
    } else {
      try {
        const params = await readParams(request);
        await revokeToken(
          rt,
          typeof params?.token === "string" ? params.token : undefined,
        );
      } catch (e) {
        console.error(e);
      }
      response = new Response(null, { status: 200 });
    }
    return withCors(response);
  }

  return { handle, matches };
}
