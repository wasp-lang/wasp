import { expect, test } from "@playwright/test";

/**
 * The spine of a hand-written scheme with Wasp-issued credentials: signup
 * through its own route, login that signs in through the credentials facet,
 * request attribution on `/auth/me`, logout revocation, and the
 * anti-fallthrough/anti-enumeration edges.
 */

const SCHEME = "password";
const uniqueSuffix = `${Date.now()}-${Math.floor(Math.random() * 10000)}`;
const email = `login-${uniqueSuffix}@example.com`;
const password = "password1234";

test.describe.configure({ mode: "serial" });

test("signup via the scheme's own route succeeds", async ({ request }) => {
  const response = await request.post("/auth/password/signup", {
    data: { email, password },
  });
  expect(response.status()).toBe(200);
});

test("duplicate signup is rejected", async ({ request }) => {
  const response = await request.post("/auth/password/signup", {
    data: { email, password },
  });
  expect(response.status()).toBe(422);
});

test("login turns a valid password into a scheme-attributed credential", async ({
  request,
}) => {
  const response = await request.post("/auth/password/login", {
    data: { email, password },
  });
  expect(response.status()).toBe(200);
  const { credential } = (await response.json()) as { credential: string };
  expect(typeof credential).toBe("string");

  const me = await request.get("/auth/me", {
    headers: { Authorization: `Bearer ${credential}` },
  });
  expect(me.status()).toBe(200);
  const user = (await me.json()) as {
    json: { credentialScheme: string; loginScheme: string };
  };
  expect(user.json.credentialScheme).toBe(SCHEME);
  expect(user.json.loginScheme).toBe(SCHEME);
});

test("a wrong password is a hard 401, with no fallthrough", async ({
  request,
}) => {
  const response = await request.post("/auth/password/login", {
    data: { email, password: "wrong-password1" },
  });
  expect(response.status()).toBe(401);
});

test("an unknown subject is a 401 indistinguishable from a wrong password", async ({
  request,
}) => {
  const response = await request.post("/auth/password/login", {
    data: { email: `nobody-${uniqueSuffix}@example.com`, password },
  });
  expect(response.status()).toBe(401);
});

test("a made-up credential is rejected", async ({ request }) => {
  const me = await request.get("/auth/me", {
    headers: { Authorization: "Bearer not-a-credential" },
  });
  expect(me.status()).toBe(401);
});

test("routes of a scheme the app does not configure are a 404", async ({
  request,
}) => {
  const response = await request.post("/auth/wasp/username/login", {
    data: { username: email, password },
  });
  expect(response.status()).toBe(404);
});

test("logout revokes the credential server-side", async ({ request }) => {
  const login = await request.post("/auth/password/login", {
    data: { email, password },
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
