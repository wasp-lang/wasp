// The middleware over a real HTTP server, the way the Wasp server mounts it.
import { createServer, type Server } from "node:http";
import type { AddressInfo } from "node:net";
import { afterEach, describe, expect, it } from "vitest";
import { createTestApp, type TestApp } from "./helpers";

let server: Server | undefined;
afterEach(
  () =>
    new Promise<void>((resolve) =>
      server ? server.close(() => resolve()) : resolve(),
    ),
);

/** Mounts the middleware the way the Wasp server does, before any body parser, with a fallback route after it. */
async function listen(app: TestApp) {
  server = createServer(async (req, res) => {
    await app.mcp.nodeMiddleware(req, res, () => {
      res.statusCode = 200;
      res.end("app route");
    });
  });
  await new Promise<void>((resolve) => server!.listen(0, "127.0.0.1", resolve));
  return `http://127.0.0.1:${(server!.address() as AddressInfo).port}`;
}

describe("node middleware", () => {
  it("passes other routes to the app with their bodies intact", async () => {
    const base = await listen(createTestApp());
    const res = await fetch(`${base}/operations/create-task`, {
      method: "POST",
      body: "hello",
    });
    expect(await res.text()).toBe("app route");
  });

  it("serves metadata and the 401 challenge", async () => {
    const base = await listen(createTestApp());
    expect(
      (await fetch(`${base}/.well-known/oauth-authorization-server`)).status,
    ).toBe(200);
    const res = await fetch(`${base}/mcp`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: "{}",
    });
    expect(res.status).toBe(401);
    expect(res.headers.get("www-authenticate")).toContain("resource_metadata=");
  });

  it("reads form bodies on the token endpoint", async () => {
    const base = await listen(createTestApp());
    const res = await fetch(`${base}/mcp/oauth/token`, {
      method: "POST",
      headers: { "Content-Type": "application/x-www-form-urlencoded" },
      body: "grant_type=refresh_token&refresh_token=nope",
    });
    expect(await res.json()).toMatchObject({ error: "invalid_grant" });
  });

  it("refuses an oversized token request without reading it all", async () => {
    const base = await listen(createTestApp());
    const res = await fetch(`${base}/mcp/oauth/token`, {
      method: "POST",
      headers: { "Content-Type": "application/x-www-form-urlencoded" },
      body: "grant_type=refresh_token&refresh_token=" + "x".repeat(200_000),
    });
    expect(res.status).toBe(413);
  });

  it("runs MCP calls over HTTP", async () => {
    const app = createTestApp();
    const { access_token } = await app.authorize("u1");
    const base = await listen(app);
    const res = await fetch(`${base}/mcp`, {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        Accept: "application/json, text/event-stream",
        "Mcp-Protocol-Version": "2025-11-25",
        Authorization: `Bearer ${access_token}`,
      },
      body: JSON.stringify({
        jsonrpc: "2.0",
        id: 1,
        method: "tools/call",
        params: { name: "create_note", arguments: { text: "via http" } },
      }),
    });
    expect(res.status).toBe(200);
    expect(await res.text()).toContain("via http");
    expect(app.notes).toMatchObject([{ text: "via http" }]);
  });

  it("answers 401 without reading the body of an unauthenticated call", async () => {
    const base = await listen(createTestApp());
    const res = await fetch(`${base}/mcp`, {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        Accept: "application/json, text/event-stream",
      },
      body: "x".repeat(5_000_000),
    });
    expect(res.status).toBe(401);
  });
});

describe("protocol eras", () => {
  it("serves 2025-era clients too", async () => {
    const app = createTestApp();
    const { access_token } = await app.authorize("u1");
    const conn = await app.connect(access_token, { era: "legacy" });
    try {
      expect(conn.client.getProtocolEra()).toBe("legacy");
      expect(await conn.toolNames()).toContain("delete_note");
      // No URL-mode elicitation over a 2025 session: destructive calls get the link.
      expect(
        (await conn.call("delete_note", { id: "1" })).structuredContent,
      ).toMatchObject({ status: "confirmation_required" });
    } finally {
      await conn.close();
    }
  });
});
