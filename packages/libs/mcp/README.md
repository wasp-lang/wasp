# MCP Library

The runtime for Wasp's experimental MCP feature (see [`MCP-FEAT-PLAN.md`](../../../MCP-FEAT-PLAN.md)): it turns a list of tools into an MCP server that agents log in to with OAuth.

- **MCP server**: stateless Streamable HTTP on the MCP TypeScript SDK v2, serving both protocol eras. Zod input and output schemas, annotations, server instructions, a built-in `whoami` tool, and errors mapped to codes with next steps (anything with a `statusCode`, like Wasp's `HttpError`, keeps its message).
- **OAuth for MCP clients**: an authorization server using Client ID Metadata Documents (no client registration), PKCE, sealed (never stored) codes and tokens, rotating refresh tokens with reuse detection, one connection per authorization, and per-connection scopes checked on every request. Tools whose scope wasn't granted either stay listed and answer `403 insufficient_scope` (step-up) or are hidden.
- **Destructive tools wait for the human**: a pending action the user approves in the app, which then runs the tool once. Clients with URL-mode elicitation open the page themselves; every other client gets a `confirmUrl`.
- **Per-connection rate limiting**, shared across server instances through the store.

## What's inside

- `index` (`@wasp/lib-mcp`): scope helpers, view types, and `McpAppEndpoints` (what the pages call), usable anywhere.
- `node` (`@wasp/lib-mcp/node`): `createWaspMcp(config)`, `defineMcpTool` / `defineMcpToolFor<User>()` (a tool calls an operation-shaped `operation(args, { user, mcp })`; its result isn't filtered by `output`), the stores (`createPrismaStore(prisma)` over the models Wasp adds to the schema, `createMemoryStore`; codes and tokens are sealed with `secret`, never stored), and the error classes.
- `browser` (`@wasp/lib-mcp/browser`): `createMcpComponents({ call, ... })`, the consent, connections and confirm components (React, styled with a CSS module on the auth forms' variables).

`createWaspMcp` returns:

- `handle(request)`: a web-standard handler for `/mcp`, `/mcp/oauth/{token,revoke}`, and the `/.well-known` metadata, which returns `null` for any other path;
- `nodeMiddleware`: the same routes as Express/Node middleware;
- `user.*`: the functions behind the consent, connections, and confirm pages, and `user.handle(userId, endpoint, body)`, which serves the `McpAppEndpoints` with validated bodies. Each one takes the logged-in user's ID; the caller handles the session.

Read more about lib exports and versioning in the parent [README](../README.md).

## Development

```bash
corepack pnpm install
corepack pnpm test # type check + unit tests
corepack pnpm build
```

The tests run a small notes app on the lib in memory: OAuth through the real endpoints, and an MCP SDK client whose `fetch` goes straight to the handler.
