import { expect, test } from "@playwright/test";

/**
 * Wasp's own username & password flows, reached through the
 * package's routes: signup and login at the manifest's basePath, a session
 * attributed to the scheme by name, Wasp's own `/auth/me` and
 * `/auth/logout` on top.
 */

const SCHEME = "wasp";
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
  const { credential } = (await response.json()) as { credential: string };
  expect(typeof credential).toBe("string");

  const me = await request.get("/auth/me", {
    headers: { Authorization: `Bearer ${credential}` },
  });
  expect(me.status()).toBe(200);
  const user = (await me.json()) as {
    json: {
      credentialScheme: string;
      loginScheme: string;
      credentialIssuedAt: string | null;
      isCredentialFresh: boolean;
    };
  };
  expect(user.json.credentialScheme).toBe(SCHEME);
  expect(user.json.loginScheme).toBe(SCHEME);
  // A credential issued seconds ago is fresh, and says when it was issued.
  expect(user.json.isCredentialFresh).toBe(true);
  expect(
    Date.now() - new Date(user.json.credentialIssuedAt!).getTime(),
  ).toBeLessThan(60_000);
});

test("a login without remember-me is marked for the browser session only", async ({
  request,
}) => {
  const response = await request.post("/auth/wasp/username/login", {
    data: { username, password, persistent: false },
  });
  expect(response.status()).toBe(200);
  const body = (await response.json()) as {
    credential: string;
    persistent?: boolean;
  };
  expect(body.persistent).toBe(false);

  const me = await request.get("/auth/me", {
    headers: { Authorization: `Bearer ${body.credential}` },
  });
  expect(me.status()).toBe(200);
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
  const { credential } = (await login.json()) as { credential: string };

  const logout = await request.post("/auth/logout", {
    headers: { Authorization: `Bearer ${credential}` },
  });
  expect(logout.status()).toBe(200);

  const me = await request.get("/auth/me", {
    headers: { Authorization: `Bearer ${credential}` },
  });
  expect(me.status()).toBe(401);
});
