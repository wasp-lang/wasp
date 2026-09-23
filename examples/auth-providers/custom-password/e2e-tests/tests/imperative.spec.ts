import { expect, test } from "@playwright/test";

/**
 * The imperative auth API (`wasp/server/auth`) from `api()` routes: sign a
 * user in by hand, ask who is asking on a public route, sign the request
 * out, and sign out everywhere.
 */

const uniqueSuffix = `${Date.now()}-${Math.floor(Math.random() * 10000)}`;
const email = `imperative-${uniqueSuffix}@example.com`;
const password = "password1234";

test.describe.configure({ mode: "serial" });

test("signIn issues a credential for an existing user", async ({ request }) => {
  const signup = await request.post("/auth/password/signup", {
    data: { email, password },
  });
  expect(signup.status()).toBe(200);

  const signedIn = await request.post("/api/sign-in-as", { data: { email } });
  expect(signedIn.status()).toBe(200);
  const { credential } = (await signedIn.json()) as { credential: string };
  expect(typeof credential).toBe("string");

  const me = await request.get("/auth/me", {
    headers: { Authorization: `Bearer ${credential}` },
  });
  expect(me.status()).toBe(200);
  const user = (await me.json()) as { json: { loginScheme: string } };
  expect(user.json.loginScheme).toBe("password");
});

test("signIn refuses an unknown user", async ({ request }) => {
  const response = await request.post("/api/sign-in-as", {
    data: { email: `nobody-${uniqueSuffix}@example.com` },
  });
  expect(response.status()).toBe(404);
});

test("authenticate on a public route knows who is asking, and tolerates nobody", async ({
  request,
}) => {
  const anonymous = (await (await request.get("/api/whoami")).json()) as {
    userId: unknown;
  };
  expect(anonymous.userId).toBeNull();

  const { credential } = (await (
    await request.post("/api/sign-in-as", { data: { email } })
  ).json()) as { credential: string };
  const known = (await (
    await request.get("/api/whoami", {
      headers: { Authorization: `Bearer ${credential}` },
    })
  ).json()) as { userId: unknown };
  expect(known.userId).not.toBeNull();
});

test("signOut ends the credential the request carries", async ({ request }) => {
  const { credential } = (await (
    await request.post("/api/sign-in-as", { data: { email } })
  ).json()) as { credential: string };
  const headers = { Authorization: `Bearer ${credential}` };
  expect((await request.post("/api/sign-out", { headers })).status()).toBe(200);
  expect((await request.get("/auth/me", { headers })).status()).toBe(401);
  // An anonymous sign-out is a no-op, not an error.
  expect((await request.post("/api/sign-out")).status()).toBe(200);
});

test("signOutEverywhere ends every credential of the user", async ({
  request,
}) => {
  const issue = async () =>
    (
      (await (
        await request.post("/api/sign-in-as", { data: { email } })
      ).json()) as {
        credential: string;
      }
    ).credential;
  const first = { Authorization: `Bearer ${await issue()}` };
  const second = { Authorization: `Bearer ${await issue()}` };
  expect((await request.get("/auth/me", { headers: second })).status()).toBe(
    200,
  );

  expect(
    (
      await request.post("/api/sign-out-everywhere", { headers: first })
    ).status(),
  ).toBe(200);
  expect((await request.get("/auth/me", { headers: first })).status()).toBe(
    401,
  );
  expect((await request.get("/auth/me", { headers: second })).status()).toBe(
    401,
  );
});
