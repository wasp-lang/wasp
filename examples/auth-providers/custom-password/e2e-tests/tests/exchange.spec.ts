import { expect, test } from "@playwright/test";

/**
 * The previously untested spine of external-provider auth: the credential
 * exchange (`POST /auth/login/:providerId`), session attribution, logout
 * revocation, and the anti-fallthrough/anti-enumeration edges.
 */

const PROVIDER_ID = "external:password";
const uniqueSuffix = `${Date.now()}-${Math.floor(Math.random() * 10000)}`;
const email = `exchange-${uniqueSuffix}@example.com`;
const password = "password1234";

function basicCredential(user: string, pass: string): string {
  return `Basic ${Buffer.from(`${user}:${pass}`).toString("base64")}`;
}

test.describe.configure({ mode: "serial" });

test("signup via the provider's own route succeeds", async ({ request }) => {
  const response = await request.post("/password-auth/signup", {
    data: { email, password },
  });
  expect(response.status()).toBe(200);
});

test("duplicate signup is rejected", async ({ request }) => {
  const response = await request.post("/password-auth/signup", {
    data: { email, password },
  });
  expect(response.status()).toBe(422);
});

test("the exchange turns a valid credential into a provider-attributed Wasp session", async ({
  request,
}) => {
  const response = await request.post(
    `/auth/login/${encodeURIComponent(PROVIDER_ID)}`,
    { headers: { Authorization: basicCredential(email, password) } },
  );
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

test("a wrong password is a hard 401, with no fallthrough", async ({
  request,
}) => {
  const response = await request.post(
    `/auth/login/${encodeURIComponent(PROVIDER_ID)}`,
    { headers: { Authorization: basicCredential(email, "wrong-password1") } },
  );
  expect(response.status()).toBe(401);
});

test("an unknown subject is a 401 indistinguishable from a wrong password", async ({
  request,
}) => {
  const response = await request.post(
    `/auth/login/${encodeURIComponent(PROVIDER_ID)}`,
    {
      headers: {
        Authorization: basicCredential(
          `nobody-${uniqueSuffix}@example.com`,
          password,
        ),
      },
    },
  );
  expect(response.status()).toBe(401);
});

test("an unknown provider id is a 404", async ({ request }) => {
  const response = await request.post("/auth/login/external:no-such-provider", {
    headers: { Authorization: basicCredential(email, password) },
  });
  expect(response.status()).toBe(404);
});

test("'wasp' is excluded from the exchange", async ({ request }) => {
  const response = await request.post("/auth/login/wasp", {
    headers: { Authorization: basicCredential(email, password) },
  });
  expect(response.status()).toBe(404);
});

test("logout revokes the session server-side", async ({ request }) => {
  const login = await request.post(
    `/auth/login/${encodeURIComponent(PROVIDER_ID)}`,
    { headers: { Authorization: basicCredential(email, password) } },
  );
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
