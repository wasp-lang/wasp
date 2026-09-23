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

test("a one-time code stands in for the credential on a navigation, once", async ({
  request,
}) => {
  const login = await request.post("/auth/password/login", {
    data: { email, password },
  });
  const { credential } = (await login.json()) as { credential: string };
  const headers = { Authorization: `Bearer ${credential}` };
  const issued = await request.post("/auth/password/one-time-code", {
    headers,
  });
  expect(issued.status()).toBe(200);
  const { oneTimeCode } = (await issued.json()) as { oneTimeCode: string };
  expect(typeof oneTimeCode).toBe("string");
  expect(oneTimeCode).not.toBe(credential);

  // It is not a credential: no route accepts it as a bearer token.
  const asBearer = await request.get("/auth/me", {
    headers: { Authorization: `Bearer ${oneTimeCode}` },
  });
  expect(asBearer.status()).toBe(401);

  // The navigation carries no header, only the code.
  const download = await request.get(
    `/auth/password/export?oneTimeCode=${encodeURIComponent(oneTimeCode)}`,
  );
  expect(download.status()).toBe(200);
  const { authId } = (await download.json()) as { authId: string };
  expect(typeof authId).toBe("string");

  // Spent: the same code does not work twice.
  const replay = await request.get(
    `/auth/password/export?oneTimeCode=${encodeURIComponent(oneTimeCode)}`,
  );
  expect(replay.status()).toBe(401);

  // The credential it stood for is untouched.
  expect((await request.get("/auth/me", { headers })).status()).toBe(200);
});

test("no one-time code without a credential, and a made-up code is rejected", async ({
  request,
}) => {
  expect((await request.post("/auth/password/one-time-code")).status()).toBe(
    401,
  );
  expect(
    (await request.get("/auth/password/export?oneTimeCode=made-up")).status(),
  ).toBe(401);
});
