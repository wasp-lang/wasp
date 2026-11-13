# Linkly with Wasp's MCP support

A port of [Linkly](https://github.com/vincanger/linkly-api-mcp-cli), the example app from the post _How to make your app usable by AI agents_, onto Wasp's experimental MCP support ([`MCP-FEAT-PLAN.md`](../../MCP-FEAT-PLAN.md)). It runs on the dev Wasp CLI from this checkout.

Linkly's hand-written MCP server, OAuth server (CIMD), and tool wrappers are gone. What's left is what a Wasp developer writes:

| What                                                                                                                                   | Where                                                                                                                                                                                   |
| -------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `app.mcp`: instructions, the consent route, the connection model                                                                       | [`main.wasp.ts`](main.wasp.ts)                                                                                                                                                          |
| Tools: `expose.mcp` on the operations (name, description, zod input/output, hints)                                                     | each slice's `*.wasp.ts` ([links](src/links/links.wasp.ts), [analytics](src/analytics/analytics.wasp.ts), [docs](src/docs/docs.wasp.ts), [deep links](src/deeplinks/deeplinks.wasp.ts)) |
| The tools' schemas                                                                                                                     | each slice's `schemas.ts`                                                                                                                                                               |
| The operations: the dashboard's own, plus MCP-only ones (`expose.client: false`) for docs and dashboard URLs. They read `context.mcp`. | each slice's `operations.ts`, [`src/deeplinks/dashboardUrl.ts`](src/deeplinks/dashboardUrl.ts), [`callerPrincipal`](src/shared/waspSession.ts)                                          |
| The consent, connections and confirm pages, around Wasp's `<McpConsentForm />`, `<McpConnections />` and `<McpConfirmAction />`        | [`src/oauth/ui/`](src/oauth/ui/)                                                                                                                                                        |

This is release step 4 of the feature, plus the fixes from the pre-PR review. Wasp adds the MCP models (`McpConnection`, `McpPendingAction`, `McpRateLimit`) to the schema itself, so Linkly declares none. Linkly's three scopes (`links:read`, `links:write`, `analytics:read`) are in `app.mcp.scopes`, and each tool names the one it needs: users tick them on the consent screen and can narrow them on the connections page, and a client that calls a tool it wasn't granted is asked to get that scope (step-up: `403 insufficient_scope`, then the consent screen again). The services also check the granted scopes, from `context.mcp` ([`callerPrincipal`](src/shared/waspSession.ts)). `delete_link` is destructive: it waits for the user's approval on the confirm page (`app.mcp.confirmRoute`, `<McpConfirmAction />`), unless they turned that off for the connection. Clients that support URL-mode elicitation open the page themselves; others get a link. The page shows [`describeDeleteLink`](src/links/describeDeleteLink.ts)'s summary (`destructive: { describe }`).

Linkly's REST API, personal access tokens, CLI, and discovery files (`llms.txt`, API catalog) are left out: they're outside v1 of the feature.

## Run it

Requirements: Node 26+, Postgres, and the dev Wasp CLI (`mise run build`; then use `../../waspc/tools/wasp-cli-dev` as `wasp`).

```bash
cp .env.server.example .env.server # add DATABASE_URL if you don't use `wasp start db`
wasp start db                      # or any Postgres; keep it running
wasp db migrate-dev
wasp start # client on :3000, server on :3001, MCP at :3001/mcp
```

## Connect an AI assistant

```bash
claude mcp add --transport http linkly http://localhost:3001/mcp
```

Your browser opens Linkly's consent screen. Log in, pick permissions, and click Allow. Manage connections at `/settings/connections`.

Web-hosted clients such as claude.ai and ChatGPT need Linkly on a public HTTPS URL (for example through a tunnel), with `WASP_SERVER_URL` and `WASP_WEB_CLIENT_URL` set to the public URLs.

## Tests

```bash
npm test         # service unit tests (Vitest, against TEST_DATABASE_URL, default linkly_mcp_test)
npm run test:e2e # Playwright dashboard tests (the OAuth and MCP browser tests aren't ported yet)
```

The MCP and OAuth behavior is tested in the lib (`cd ../../packages/libs/mcp && corepack pnpm test`) and in Wasp's `wasp-mcp` snapshot test.

## License

[MIT](LICENSE)
