import { describe, expect, it } from "vitest";
import { createMemoryStore } from "../src/node";
import {
  CLIENT_ID,
  createTestApp,
  FRONTEND_URL,
  REDIRECT_URI,
  SERVER_URL,
} from "./helpers";

const tokenRequest = (params: Record<string, string>) => ({
  method: "POST",
  headers: { "Content-Type": "application/x-www-form-urlencoded" },
  body: new URLSearchParams(params),
});

describe("discovery", () => {
  it("answers an unauthenticated MCP request with 401 pointing at the metadata", async () => {
    const app = createTestApp({ initialScopes: ["notes:read"] });
    const res = await app.rawCall(null, "list_notes");
    expect(res.status).toBe(401);
    expect(res.headers.get("WWW-Authenticate")).toBe(
      `Bearer resource_metadata="${SERVER_URL}/.well-known/oauth-protected-resource/mcp", scope="notes:read"`,
    );
  });

  it("serves protected-resource and authorization-server metadata", async () => {
    const app = createTestApp({ initialScopes: ["notes:read"] });
    const resource = await (
      await app.send("/.well-known/oauth-protected-resource/mcp")
    ).json();
    expect(resource).toMatchObject({
      resource: `${SERVER_URL}/mcp`,
      authorization_servers: [SERVER_URL],
      scopes_supported: ["notes:read"],
    });
    const server = await (
      await app.send("/.well-known/oauth-authorization-server")
    ).json();
    expect(server).toMatchObject({
      issuer: SERVER_URL,
      authorization_endpoint: `${FRONTEND_URL}/mcp/connect`,
      token_endpoint: `${SERVER_URL}/mcp/oauth/token`,
      code_challenge_methods_supported: ["S256"],
      client_id_metadata_document_supported: true,
      scopes_supported: ["notes:read", "notes:write"],
    });
  });

  it("answers CORS preflights and leaves other paths to the app", async () => {
    const app = createTestApp();
    const preflight = await app.send("/mcp", { method: "OPTIONS" });
    expect(preflight.status).toBe(204);
    expect(preflight.headers.get("Access-Control-Allow-Origin")).toBe("*");
    expect(
      await app.mcp.handle(new Request(`${SERVER_URL}/operations/get-tasks`)),
    ).toBeNull();
  });
});

describe("consent", () => {
  const params = {
    response_type: "code",
    client_id: CLIENT_ID,
    redirect_uri: REDIRECT_URI,
    code_challenge: "a".repeat(43),
    code_challenge_method: "S256",
    state: "xyz",
    scope: "notes:read",
  };

  it("describes the request with the client's verified domain and every scope", async () => {
    const view = await createTestApp().mcp.user.describeAuthorizeRequest(
      "u1",
      params,
    );
    expect(view).toMatchObject({
      ok: true,
      clientName: "Test Client",
      clientHost: "client.example",
      requestedScopes: ["notes:read"],
      existingConnection: null,
    });
    expect(view.ok && view.scopes.map((s) => s.scope)).toEqual([
      "notes:read",
      "notes:write",
    ]);
  });

  it("never redirects to an unregistered redirect_uri", async () => {
    const app = createTestApp();
    await expect(
      app.mcp.user.describeAuthorizeRequest("u1", {
        ...params,
        redirect_uri: "https://evil.example/cb",
      }),
    ).rejects.toThrow(/redirect_uri isn't one of/);
  });

  it("redirects other problems back to the client", async () => {
    const view = await createTestApp().mcp.user.describeAuthorizeRequest("u1", {
      ...params,
      scope: "notes:delete",
    });
    expect(view).toMatchObject({ ok: false, error: "invalid_scope" });
    expect(!view.ok && new URL(view.redirectTo).searchParams.get("state")).toBe(
      "xyz",
    );
  });

  it("refuses localhost client IDs unless allowed", async () => {
    const app = createTestApp();
    await expect(
      app.mcp.user.describeAuthorizeRequest("u1", {
        ...params,
        client_id: "http://localhost:5555/client.json",
      }),
    ).rejects.toThrow(/localhost/);
  });

  it("sends the user back with access_denied when they deny", async () => {
    const { redirectTo } = await createTestApp().mcp.user.denyAuthorization(
      "u1",
      { params },
    );
    const url = new URL(redirectTo);
    expect(url.searchParams.get("error")).toBe("access_denied");
    expect(url.searchParams.get("iss")).toBe(SERVER_URL);
  });
});

describe("tokens", () => {
  it("issues tokens for an approved request, with implied scopes", async () => {
    const app = createTestApp();
    const tokens = await app.authorize("u1", { scopes: ["notes:write"] });
    expect(tokens.access_token).toMatch(/^mcp_at_/);
    expect(tokens.refresh_token).toMatch(/^mcp_rt_/);
    expect(tokens.scope).toBe("notes:read notes:write");
    expect(await app.mcp.user.listConnections("u1")).toMatchObject([
      { clientName: "Test Client", scopes: ["notes:read", "notes:write"] },
    ]);
  });

  it("rejects a wrong PKCE verifier", async () => {
    const app = createTestApp();
    const { code } = await app.authorize("u1");
    // The code was already used by authorize(); a fresh flow is needed for this check.
    const fresh = await app.mcp.user.approveAuthorization("u1", {
      params: {
        response_type: "code",
        client_id: CLIENT_ID,
        redirect_uri: REDIRECT_URI,
        code_challenge: "b".repeat(43),
        code_challenge_method: "S256",
      },
      scopes: ["notes:read"],
      requireConfirmation: true,
    });
    const freshCode = new URL(fresh.redirectTo).searchParams.get("code")!;
    expect(freshCode).not.toBe(code);
    const res = await app.send(
      "/mcp/oauth/token",
      tokenRequest({
        grant_type: "authorization_code",
        code: freshCode,
        redirect_uri: REDIRECT_URI,
        client_id: CLIENT_ID,
        code_verifier: "wrong".repeat(10),
      }),
    );
    expect(res.status).toBe(400);
    expect(await res.json()).toMatchObject({
      error: "invalid_grant",
      error_description: "PKCE verification failed.",
    });
  });

  it("revokes the connection when a code is used twice", async () => {
    const app = createTestApp();
    const first = await app.authorize("u1");
    const again = await app.send(
      "/mcp/oauth/token",
      tokenRequest({
        grant_type: "authorization_code",
        code: first.code,
        redirect_uri: REDIRECT_URI,
        client_id: CLIENT_ID,
        code_verifier: first.verifier,
      }),
    );
    expect(await again.json()).toMatchObject({ error: "invalid_grant" });
    expect(await app.mcp.user.listConnections("u1")).toEqual([]);
    expect((await app.rawCall(first.access_token, "list_notes")).status).toBe(
      401,
    );
  });

  it("rotates refresh tokens and treats reuse as theft", async () => {
    const app = createTestApp();
    const { refresh_token } = await app.authorize("u1");
    const rotated = await (
      await app.send(
        "/mcp/oauth/token",
        tokenRequest({ grant_type: "refresh_token", refresh_token }),
      )
    ).json();
    expect(rotated.access_token).toMatch(/^mcp_at_/);
    expect(rotated.refresh_token).not.toBe(refresh_token);

    const reused = await app.send(
      "/mcp/oauth/token",
      tokenRequest({ grant_type: "refresh_token", refresh_token }),
    );
    expect(await reused.json()).toMatchObject({ error: "invalid_grant" });
    expect((await app.rawCall(rotated.access_token, "list_notes")).status).toBe(
      401,
    );
  });

  it("revokes an access token at the revocation endpoint", async () => {
    const app = createTestApp();
    const { access_token } = await app.authorize("u1");
    expect(
      (
        await app.send(
          "/mcp/oauth/revoke",
          tokenRequest({ token: access_token }),
        )
      ).status,
    ).toBe(200);
    expect((await app.rawCall(access_token, "list_notes")).status).toBe(401);
  });

  it("rejects tampered tokens, tokens of another kind, and another secret's tokens", async () => {
    const app = createTestApp();
    const { access_token, refresh_token } = await app.authorize("u1");
    const flipped =
      access_token.slice(0, -2) + (access_token.endsWith("A") ? "BB" : "AA");
    expect((await app.rawCall(flipped, "list_notes")).status).toBe(401);
    expect((await app.rawCall(refresh_token, "list_notes")).status).toBe(401);
    const other = createTestApp({
      secret: "another-secret-that-is-at-least-32-bytes",
    });
    expect((await other.rawCall(access_token, "list_notes")).status).toBe(401);
    expect((await app.rawCall(access_token, "list_notes")).status).toBe(200);
  });

  it("stores no codes or tokens, only the connection", async () => {
    const app = createTestApp();
    await app.authorize("u1");
    expect([...app.store.records.connections.values()]).toMatchObject([
      { userId: "u1", refreshGeneration: 0 },
    ]);
  });

  it("doesn't revoke anything when a replayed code has the wrong verifier", async () => {
    const app = createTestApp();
    const first = await app.authorize("u1");
    const replay = await app.send(
      "/mcp/oauth/token",
      tokenRequest({
        grant_type: "authorization_code",
        code: first.code,
        redirect_uri: REDIRECT_URI,
        client_id: CLIENT_ID,
        code_verifier: "wrong".repeat(10),
      }),
    );
    expect(await replay.json()).toMatchObject({
      error: "invalid_grant",
      error_description: "PKCE verification failed.",
    });
    expect((await app.rawCall(first.access_token, "list_notes")).status).toBe(
      200,
    );
  });

  it("revokes only the connection a reused code made, and never makes another", async () => {
    const app = createTestApp();
    await app.authorize("u1", { scopes: ["notes:read"] });
    const second = await app.authorize("u1", { scopes: ["notes:write"] });
    const reuse = () =>
      app.send(
        "/mcp/oauth/token",
        tokenRequest({
          grant_type: "authorization_code",
          code: second.code,
          redirect_uri: REDIRECT_URI,
          client_id: CLIENT_ID,
          code_verifier: second.verifier,
        }),
      );
    expect(await (await reuse()).json()).toMatchObject({
      error: "invalid_grant",
    });
    // A third use finds the revoked connection, so it can't make a new one.
    expect(await (await reuse()).json()).toMatchObject({
      error: "invalid_grant",
    });
    // The first authorization is untouched.
    expect(await app.mcp.user.listConnections("u1")).toMatchObject([
      { scopes: ["notes:read"] },
    ]);
  });

  it("keeps two installs of one client working", async () => {
    const app = createTestApp();
    const laptop = await app.authorize("u1");
    const desktop = await app.authorize("u1");
    for (const install of [laptop, desktop]) {
      const res = await app.send(
        "/mcp/oauth/token",
        tokenRequest({
          grant_type: "refresh_token",
          refresh_token: install.refresh_token,
          client_id: CLIENT_ID,
        }),
      );
      expect(res.status).toBe(200);
    }
    // One entry on the connections page; disconnecting it ends both.
    const listed = await app.mcp.user.listConnections("u1");
    expect(listed).toHaveLength(1);
    await app.mcp.user.revokeConnection("u1", {
      clientId: listed[0]!.clientId,
    });
    for (const install of [laptop, desktop])
      expect(
        (await app.rawCall(install.access_token, "list_notes")).status,
      ).toBe(401);
  });

  it("refuses redirect URIs that aren't https or loopback http", async () => {
    for (const redirect of [
      "javascript:alert(1)//",
      "http://evil.example/cb",
    ]) {
      const app = createTestApp({
        fetchClientMetadata: async (url) => ({
          client_id: url.toString(),
          client_name: "Evil",
          redirect_uris: [redirect],
        }),
      });
      await expect(
        app.mcp.user.describeAuthorizeRequest("u1", {
          response_type: "x",
          client_id: CLIENT_ID,
          redirect_uri: redirect,
        }),
      ).rejects.toThrow(/must use https/);
    }
  });

  it("gives connections made before any scopes were declared every scope", async () => {
    // An app on one all-or-nothing consent: no scopes declared.
    const store = createMemoryStore();
    const before = createTestApp({ store, scopes: {}, tools: [] });
    const { access_token } = await before.authorize("u1", { scopes: [] });
    expect([...store.records.connections.values()]).toMatchObject([
      { scopes: [], requestedScopes: [] },
    ]);
    // The app declares scopes; the same token now sees every scoped tool.
    const after = createTestApp({ store, insufficientScope: "hide" });
    const conn = await after.connect(access_token);
    expect((await conn.toolNames()).sort()).toEqual([
      "create_note",
      "delete_note",
      "explode",
      "list_notes",
      "whoami",
    ]);
    expect(await after.mcp.user.listConnections("u1")).toMatchObject([
      { scopes: ["notes:read", "notes:write"] },
    ]);
    await conn.close();
  });

  it("keeps one connection per user and client, updated on re-consent", async () => {
    const app = createTestApp();
    await app.authorize("u1", { scopes: ["notes:read"] });
    await app.authorize("u1", {
      scopes: ["notes:write"],
      requireConfirmation: false,
    });
    expect(await app.mcp.user.listConnections("u1")).toMatchObject([
      { scopes: ["notes:read", "notes:write"], requireConfirmation: false },
    ]);
  });
});

describe("connections", () => {
  it("applies narrowed scopes and revocation on the next call", async () => {
    const app = createTestApp();
    const { access_token } = await app.authorize("u1", {
      scopes: ["notes:write"],
    });
    const [connection] = await app.mcp.user.listConnections("u1");

    await app.mcp.user.updateConnection("u1", {
      clientId: connection!.clientId,
      scopes: ["notes:read"],
    });
    expect(
      (await app.rawCall(access_token, "create_note", { text: "hi" })).status,
    ).toBe(403);

    await app.mcp.user.revokeConnection("u1", {
      clientId: connection!.clientId,
    });
    expect((await app.rawCall(access_token, "list_notes")).status).toBe(401);
  });

  it("never grants more than the client asked for", async () => {
    const app = createTestApp();
    await app.authorize("u1", {
      scopes: ["notes:read"],
      requested: ["notes:read"],
    });
    const [connection] = await app.mcp.user.listConnections("u1");
    const updated = await app.mcp.user.updateConnection("u1", {
      clientId: connection!.clientId,
      scopes: ["notes:write"],
    });
    expect(updated.scopes).toEqual(["notes:read"]);
  });

  it("keeps a narrowed connection from before the app had scopes narrowed", async () => {
    const store = createMemoryStore();
    const before = createTestApp({ store, scopes: {}, tools: [] });
    const { access_token } = await before.authorize("u1", { scopes: [] });
    const after = createTestApp({ store });
    const [legacy] = await after.mcp.user.listConnections("u1");
    expect(legacy!.scopes).toEqual(["notes:read", "notes:write"]);
    await after.mcp.user.updateConnection("u1", {
      clientId: legacy!.clientId,
      scopes: ["notes:read"],
    });
    expect((await after.mcp.user.listConnections("u1"))[0]!.scopes).toEqual([
      "notes:read",
    ]);
    expect(
      (await after.rawCall(access_token, "create_note", { text: "hi" })).status,
    ).toBe(403);
  });

  it("serves the app endpoints with validated bodies", async () => {
    const app = createTestApp();
    await app.authorize("u1");
    const listed = (await app.mcp.user.handle(
      "u1",
      "connections/list",
      {},
    )) as {
      connections: unknown[];
    };
    expect(listed.connections).toHaveLength(1);
    await expect(
      app.mcp.user.handle("u1", "connections/update", { clientId: 42 }),
    ).rejects.toThrow(/Invalid request body/);
    await expect(app.mcp.user.handle("u1", "nope", {})).rejects.toThrow(
      /Not found/,
    );
  });

  it("hides other users' connections", async () => {
    const app = createTestApp();
    await app.authorize("u1");
    const [connection] = await app.mcp.user.listConnections("u1");
    await expect(
      app.mcp.user.revokeConnection("u2", { clientId: connection!.clientId }),
    ).rejects.toThrow(/not found/);
  });
});
