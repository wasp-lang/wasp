import { expect, test } from "@playwright/test";

/**
 * Wasp's own username & password flows, reached through the
 * package's routes: signup and login at the manifest's basePath, a session
 * attributed to the package's provider id, Wasp's own `/auth/me` and
 * `/auth/logout` on top.
 */

const PROVIDER_ID = "wasp";
const uniqueSuffix = `${Date.now()}-${Math.floor(Math.random() * 10000)}`;
const username = `alice-${uniqueSuffix}`;
const password = "password1234";

test.describe.configure({ mode: "serial" });

test("signup through the package's routes succeeds", async ({ request }) => {
  const response = await request.post("/auth/wasp/username/signup", {
    data: { username, password },
  });
  expect(response.status()).toBe(200);
  expect(await response.json()).toEqual({ success: true });
});

test("duplicate signup is rejected", async ({ request }) => {
  const response = await request.post("/auth/wasp/username/signup", {
    data: { username, password },
  });
  expect(response.status()).toBe(422);
});

test("login mints a Wasp session attributed to the package", async ({
  request,
}) => {
  const response = await request.post("/auth/wasp/username/login", {
    data: { username, password },
  });
  expect(response.status()).toBe(200);
  const { sessionId } = (await response.json()) as { sessionId: string };
  expect(typeof sessionId).toBe("string");

  const me = await request.get("/auth/me", {
    headers: { Authorization: `Bearer ${sessionId}` },
  });
  expect(me.status()).toBe(200);
  const user = (await me.json()) as { json: { sessionProviderId: string } };
  expect(user.json.sessionProviderId).toBe(PROVIDER_ID);
});

test("a wrong password is a 401", async ({ request }) => {
  const response = await request.post("/auth/wasp/username/login", {
    data: { username, password: "wrong-password1" },
  });
  expect(response.status()).toBe(401);
});

test("the old unprefixed route is gone: nothing but the package mounts the flows", async ({
  request,
}) => {
  const response = await request.post("/auth/username/login", {
    data: { username, password },
  });
  expect(response.status()).toBe(404);
});

test("logout revokes the session server-side", async ({ request }) => {
  const login = await request.post("/auth/wasp/username/login", {
    data: { username, password },
  });
  const { sessionId } = (await login.json()) as { sessionId: string };

  const logout = await request.post("/auth/logout", {
    headers: { Authorization: `Bearer ${sessionId}` },
  });
  expect(logout.status()).toBe(200);

  const me = await request.get("/auth/me", {
    headers: { Authorization: `Bearer ${sessionId}` },
  });
  expect(me.status()).toBe(401);
});
