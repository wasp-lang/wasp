import express from "express";
import type { Server } from "node:http";
import path from "node:path";
import { afterEach, describe, expect, it } from "vitest";
import {
  configureBrowserAppDelivery,
  type DeliveryStorage,
} from "../src/browser";
import { configureAppDelivery } from "../src/node";

const servers: Server[] = [];

afterEach(() => {
  for (const server of servers.splice(0)) {
    server.close();
  }
});

describe("AppDelivery", () => {
  it("mounts integrated Wasp API and custom routes without leaking Wasp API paths", async () => {
    const app = express();
    const waspApi = express.Router();
    const custom = express.Router();
    const serverRoot = express.Router();
    waspApi.get("/status", (_request, response) =>
      response.json({ route: "waspApi" }),
    );
    waspApi.get("/wasp-only", (_request, response) =>
      response.json({ route: "waspApi" }),
    );
    custom.get("/api/status", (_request, response) =>
      response.json({ route: "custom" }),
    );

    configureAppDelivery({
      mode: "integrated",
      serverUrl: "https://server.test",
      waspApiMountPath: "/api",
      authEnabled: false,
      serveClientAssets: false,
    }).mount({
      app,
      routes: { waspApi, custom, serverRoot },
    });

    const server = await listen(app);
    const baseUrl = getBaseUrl(server);

    expect((await fetch(`${baseUrl}/health`)).status).toBe(200);
    expect(await (await fetch(`${baseUrl}/api/status`)).json()).toEqual({
      route: "custom",
    });
    expect(await (await fetch(`${baseUrl}/api/wasp-only`)).json()).toEqual({
      route: "waspApi",
    });
    expect((await fetch(`${baseUrl}/status`)).status).toBe(404);
  });

  it("gives custom routes precedence in split mode", async () => {
    const app = express();
    const waspApi = express.Router();
    const custom = express.Router();
    const serverRoot = express.Router();
    waspApi.get("/status", (_request, response) =>
      response.json({ route: "waspApi" }),
    );
    waspApi.get("/wasp-only", (_request, response) =>
      response.json({ route: "waspApi" }),
    );
    custom.get("/api/status", (_request, response) =>
      response.json({ route: "custom" }),
    );

    configureAppDelivery({
      mode: "split",
      serverUrl: "https://server.test",
      waspApiMountPath: "/api",
      authEnabled: false,
      serveClientAssets: false,
    }).mount({
      app,
      routes: { waspApi, custom, serverRoot },
    });

    const server = await listen(app);
    const baseUrl = getBaseUrl(server);

    expect(await (await fetch(`${baseUrl}/api/status`)).json()).toEqual({
      route: "custom",
    });
    expect(await (await fetch(`${baseUrl}/api/wasp-only`)).json()).toEqual({
      route: "waspApi",
    });
  });

  it("protects integrated session mutations and exposes the session cookie transport", async () => {
    const app = express();
    const waspApi = express.Router();
    const custom = express.Router();
    const serverRoot = express.Router();
    const delivery = configureAppDelivery({
      mode: "integrated",
      serverUrl: "https://server.test",
      waspApiMountPath: "/api",
      authEnabled: true,
      serveClientAssets: false,
    });
    waspApi.post("/login", (_request, response) => {
      delivery.respondWithSession(response, "session-id");
      response.json({ sessionId: "session-id" });
    });
    custom.post("/mutate", (_request, response) => response.json({ ok: true }));
    delivery.mount({ app, routes: { waspApi, custom, serverRoot } });

    const server = await listen(app);
    const baseUrl = getBaseUrl(server);
    const rejected = await fetch(`${baseUrl}/mutate`, {
      method: "POST",
      headers: { Cookie: "wasp_session=session-id" },
    });
    const login = await fetch(`${baseUrl}/api/login`, { method: "POST" });

    expect(rejected.status).toBe(403);
    expect(login.status).toBe(200);
    expect(login.headers.get("set-cookie")).toContain(
      "wasp_session=session-id",
    );
  });

  it("reads credentials according to the configured transport", () => {
    const integrated = configureAppDelivery({
      mode: "integrated",
      serverUrl: "https://server.test",
      waspApiMountPath: "/api",
      authEnabled: false,
      serveClientAssets: false,
    });
    const split = configureAppDelivery({
      mode: "split",
      serverUrl: "https://server.test",
      waspApiMountPath: "/api",
      authEnabled: false,
      serveClientAssets: false,
    });

    expect(integrated.waspApiPath("auth/me")).toBe("/api/auth/me");
    expect(split.waspApiPath("/auth/me")).toBe("/api/auth/me");
    expect(integrated.waspApiUrl("/auth/google/login")).toBe(
      "https://server.test/api/auth/google/login",
    );

    expect(
      integrated.readHttpSessionCredential({
        get: () => "",
        headers: { cookie: "wasp_session=cookie-id" },
      } as never),
    ).toBe("cookie-id");
    expect(
      split.readHttpSessionCredential({
        get: () => "Bearer bearer-id",
        headers: {},
      } as never),
    ).toBe("bearer-id");
    expect(
      integrated.readHttpSessionCredential({
        get: () => "Bearer bearer-id",
        headers: {},
      } as never),
    ).toBeNull();
    expect(
      split.readHttpSessionCredential({
        get: () => "",
        headers: { cookie: "wasp_session=cookie-id" },
      } as never),
    ).toBeNull();
    expect(
      integrated.readSocketSessionCredential({
        headers: { cookie: "wasp_session=cookie-id" },
        auth: {},
      }),
    ).toBe("cookie-id");
    expect(
      split.readSocketSessionCredential({
        headers: {},
        auth: { sessionId: "socket-id" },
      }),
    ).toBe("socket-id");
  });

  it("clears invalid integrated cookies but rejects invalid split credentials", async () => {
    const app = express();
    const integrated = configureAppDelivery({
      mode: "integrated",
      serverUrl: "https://server.test",
      waspApiMountPath: "/api",
      authEnabled: false,
      serveClientAssets: false,
    });
    const split = configureAppDelivery({
      mode: "split",
      serverUrl: "https://server.test",
      waspApiMountPath: "/api",
      authEnabled: false,
      serveClientAssets: false,
    });
    app.get("/integrated", (_request, response) => {
      response.json({
        action: integrated.handleInvalidHttpSessionCredential(response),
      });
    });
    app.get("/split", (_request, response) => {
      response.json({
        action: split.handleInvalidHttpSessionCredential(response),
      });
    });

    const server = await listen(app);
    const baseUrl = getBaseUrl(server);
    const integratedResponse = await fetch(`${baseUrl}/integrated`);
    const splitResponse = await fetch(`${baseUrl}/split`);

    expect(await integratedResponse.json()).toEqual({
      action: "continueUnauthenticated",
    });
    expect(integratedResponse.headers.get("set-cookie")).toContain(
      "wasp_session=;",
    );
    expect(integratedResponse.headers.get("set-cookie")).toContain("HttpOnly");
    expect(integratedResponse.headers.get("set-cookie")).toContain("Path=/");
    expect(integratedResponse.headers.get("set-cookie")).toContain(
      "SameSite=Lax",
    );
    expect(await splitResponse.json()).toEqual({ action: "reject" });
    expect(splitResponse.headers.get("set-cookie")).toBeNull();
  });

  it("allows custom health overrides", async () => {
    const app = express();
    const custom = express.Router();
    custom.get("/health", (_request, response) =>
      response.json({ status: "custom" }),
    );
    configureAppDelivery({
      mode: "integrated",
      serverUrl: "https://server.test",
      waspApiMountPath: "/api",
      authEnabled: false,
      serveClientAssets: false,
    }).mount({
      app,
      routes: {
        waspApi: express.Router(),
        custom,
        serverRoot: express.Router(),
      },
    });

    const server = await listen(app);
    expect(await (await fetch(`${getBaseUrl(server)}/health`)).json()).toEqual({
      status: "custom",
    });
  });

  it("uses the SPA fallback only for HTML navigation requests", async () => {
    const app = express();
    configureAppDelivery({
      mode: "integrated",
      serverUrl: "https://server.test",
      waspApiMountPath: "/api",
      authEnabled: false,
      serveClientAssets: true,
    }).mount({
      app,
      routes: {
        waspApi: express.Router(),
        custom: express.Router(),
        serverRoot: express.Router(),
      },
      clientAssets: {
        directory: path.resolve("tests/fixtures"),
        fallbackFile: "200.html",
      },
    });

    const server = await listen(app);
    const baseUrl = getBaseUrl(server);

    expect(
      (await fetch(`${baseUrl}/missing.js`, { headers: { Accept: "*/*" } }))
        .status,
    ).toBe(404);
    const navigationResponse = await fetch(`${baseUrl}/client-route`, {
      headers: { Accept: "text/html" },
    });
    expect(await navigationResponse.text()).toContain("SPA fallback");
  });

  it("treats malformed cookie encoding as an invalid credential", () => {
    const delivery = configureAppDelivery({
      mode: "integrated",
      serverUrl: "https://server.test",
      waspApiMountPath: "/api",
      authEnabled: false,
      serveClientAssets: false,
    });

    expect(
      delivery.readHttpSessionCredential({
        get: () => "",
        headers: { cookie: "wasp_session=%" },
      } as never),
    ).toBe("%");
  });
});

describe("BrowserAppDelivery", () => {
  it("drives the split session lifecycle and request headers", () => {
    const storage = makeStorage();
    const delivery = configureBrowserAppDelivery({
      config: {
        mode: "split",
        serverUrl: "https://server.test",
        waspApiMountPath: "/api",
      },
      storage,
    });

    delivery.acceptSession("session-id");
    const request = new Request("https://server.test/api");
    delivery.prepareHttpRequest(request);

    expect(delivery.currentSessionId()).toBe("session-id");
    expect(delivery.serverUrl).toBe("https://server.test");
    expect(request.headers.get("Authorization")).toBe("Bearer session-id");
    expect(delivery.socketConnectionOptions()).toEqual({
      withCredentials: false,
      auth: { sessionId: "session-id" },
    });

    delivery.clearSession();
    expect(delivery.currentSessionId()).toBeNull();
  });

  it("uses the browser origin and cookies for integrated requests", () => {
    const storage = makeStorage();
    Object.defineProperty(globalThis, "window", {
      configurable: true,
      value: { location: { origin: "https://app.test" } },
    });
    Object.defineProperty(globalThis, "document", {
      configurable: true,
      value: { cookie: "wasp_csrf=csrf-token" },
    });
    const delivery = configureBrowserAppDelivery({
      config: {
        mode: "integrated",
        serverUrl: "http://localhost:3001",
        waspApiMountPath: "/api",
      },
      storage,
    });

    const request = new Request("https://app.test/api", { method: "POST" });
    delivery.prepareHttpRequest(request);

    expect(delivery.serverUrl).toBe("https://app.test");
    expect(delivery.waspApiUrl("/auth/google/login")).toBe(
      "https://app.test/api/auth/google/login",
    );
    expect(request.headers.get("X-Wasp-CSRF")).toBe("csrf-token");
    expect(delivery.socketConnectionOptions()).toEqual({
      withCredentials: true,
      auth: { sessionId: null },
    });
  });
});

function makeStorage(): DeliveryStorage {
  const values = new Map<string, string>();
  return {
    set: (key, value) => values.set(key, value),
    get: (key) => values.get(key),
    remove: (key) => values.delete(key),
    clear: () => values.clear(),
  };
}

async function listen(app: express.Express): Promise<Server> {
  const server = app.listen(0);
  await new Promise<void>((resolve) => server.once("listening", resolve));
  servers.push(server);
  return server;
}

function getBaseUrl(server: Server): string {
  const address = server.address();
  if (!address || typeof address === "string") {
    throw new Error("Expected the test server to have a TCP address");
  }

  return `http://127.0.0.1:${address.port}`;
}
