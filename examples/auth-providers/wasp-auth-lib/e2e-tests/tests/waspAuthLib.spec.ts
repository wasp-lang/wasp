import { expect, test } from "@playwright/test";

/**
 * Runtime coverage for the externalized Wasp auth (`@wasp.sh/auth`): every
 * method family, plus the app-level hooks firing at Wasp-owned choke points
 * for a PACKAGE provider.
 */

const uniqueSuffix = `${Date.now()}-${Math.floor(Math.random() * 10000)}`;
const username = `franjo-${uniqueSuffix}`;
const email = `franjo-${uniqueSuffix}@example.com`;
const password = "password1234";

test.describe.configure({ mode: "serial" });

test("the app-level onBeforeSignup veto rejects a package-route signup as a 400", async ({
  request,
}) => {
  const response = await request.post("/wasp-auth/username/signup", {
    data: { username: `blocked-${uniqueSuffix}`, password },
  });
  expect(response.status()).toBe(400);
  const body = (await response.json()) as { message: string };
  expect(body.message).toBe("This name is not allowed.");
});

test("username signup and login mint a provider-attributed session", async ({
  request,
}) => {
  const signup = await request.post("/wasp-auth/username/signup", {
    data: { username, password },
  });
  expect(signup.status()).toBe(200);

  const login = await request.post("/wasp-auth/username/login", {
    data: { username, password },
  });
  expect(login.status()).toBe(200);
  const { sessionId } = (await login.json()) as { sessionId: string };

  const me = await request.get("/auth/me", {
    headers: { Authorization: `Bearer ${sessionId}` },
  });
  expect(me.status()).toBe(200);
  const user = (await me.json()) as { json: { sessionProviderId: string } };
  expect(user.json.sessionProviderId).toBe("external:wasp-auth");
});

test("a wrong password is a 401", async ({ request }) => {
  const login = await request.post("/wasp-auth/username/login", {
    data: { username, password: "wrong-password1" },
  });
  expect(login.status()).toBe(401);
});

test("email login is refused until the emailed link verifies the address", async ({
  request,
}) => {
  const signup = await request.post("/wasp-auth/email/signup", {
    data: { email, password },
  });
  expect(signup.status()).toBe(200);

  const unverifiedLogin = await request.post("/wasp-auth/email/login", {
    data: { email, password },
  });
  expect(unverifiedLogin.status()).toBe(401);
});

test("garbage verification and one-time-code tokens are rejected", async ({
  request,
}) => {
  const verify = await request.post("/wasp-auth/email/verify", {
    data: { token: "garbage" },
  });
  expect(verify.status()).toBe(400);

  const exchange = await request.post("/wasp-auth/exchange-code", {
    data: { code: "garbage" },
  });
  expect(exchange.status()).toBe(401);
});

test("the Google login leg redirects with state, PKCE and guarded cookies", async ({
  request,
}) => {
  const response = await request.get("/wasp-auth/google/login", {
    maxRedirects: 0,
  });
  expect(response.status()).toBe(302);
  const location = response.headers()["location"] ?? "";
  expect(location).toContain("accounts.google.com");
  expect(location).toContain("state=");
  expect(location).toContain("code_challenge_method=S256");
  const cookies = response.headersArray().filter((h) => h.name.toLowerCase() === "set-cookie");
  const cookieBlob = cookies.map((c) => c.value).join("\n");
  expect(cookieBlob).toContain("wasp_auth_state=");
  expect(cookieBlob).toContain("wasp_auth_codeVerifier=");
  expect(cookieBlob).toContain("HttpOnly");
});

test("logout revokes a package-minted session server-side", async ({
  request,
}) => {
  const login = await request.post("/wasp-auth/username/login", {
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

// The email verification link and reset token ride the Dummy sender's server
// log, which these API-level specs cannot read -- the full verify + reset +
// revoke-all path is covered by the in-tree flows' kitchen-sink suite (same
// facets) and by the manual transcript in the README.
