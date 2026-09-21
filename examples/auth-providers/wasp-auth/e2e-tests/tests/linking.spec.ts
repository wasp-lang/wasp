import { expect, test, type APIRequestContext } from "@playwright/test";
import { WASP_CLIENT_URL } from "../playwright.config";

/**
 * Account linking inside Wasp's own auth: a second login attached to the
 * signed-in account through the identities facet's `link`, its guards, the
 * app's veto hook, and `unlink` refusing the last login. The browser spec
 * covers the client runtime's `fetch` (credential attached by Wasp) and
 * `refreshUser`.
 */

const uniqueSuffix = `${Date.now()}-${Math.floor(Math.random() * 10000)}`;
const password = "password1234";
const first = `carol-${uniqueSuffix}`;
const second = `carol-alt-${uniqueSuffix}`;
const stranger = `dave-${uniqueSuffix}`;

test.describe.configure({ mode: "serial" });

async function signup(request: APIRequestContext, username: string) {
  const response = await request.post("/auth/wasp/username/signup", {
    data: { username, password },
  });
  expect(response.status()).toBe(200);
}

async function login(request: APIRequestContext, username: string) {
  const response = await request.post("/auth/wasp/username/login", {
    data: { username, password },
  });
  expect(response.status()).toBe(200);
  const { credential } = (await response.json()) as { credential: string };
  return { Authorization: `Bearer ${credential}` };
}

async function me(request: APIRequestContext, headers: Record<string, string>) {
  const response = await request.get("/auth/me", { headers });
  expect(response.status()).toBe(200);
  return (
    (await response.json()) as {
      json: { id: string; identities: Array<{ providerUserId: string }> };
    }
  ).json;
}

test("linking needs a signed-in user", async ({ request }) => {
  const response = await request.post("/auth/wasp/username/link", {
    data: { username: second, password },
  });
  expect(response.status()).toBe(401);
});

test("a linked login signs into the same account", async ({ request }) => {
  await signup(request, first);
  const headers = await login(request, first);
  const before = await me(request, headers);

  const link = await request.post("/auth/wasp/username/link", {
    headers,
    data: { username: second, password },
  });
  expect(link.status()).toBe(200);

  const viaSecond = await me(request, await login(request, second));
  expect(viaSecond.id).toBe(before.id);
  expect(viaSecond.identities.map((i) => i.providerUserId).sort()).toEqual(
    [first, second].sort(),
  );
});

test("linking the same login again is a no-op", async ({ request }) => {
  const response = await request.post("/auth/wasp/username/link", {
    headers: await login(request, first),
    data: { username: second, password },
  });
  expect(response.status()).toBe(200);
});

test("a login that belongs to another account cannot be linked", async ({
  request,
}) => {
  await signup(request, stranger);
  const response = await request.post("/auth/wasp/username/link", {
    headers: await login(request, first),
    data: { username: stranger, password },
  });
  expect(response.status()).toBe(409);
});

test("the app's onBeforeLink hook can veto a link", async ({ request }) => {
  const response = await request.post("/auth/wasp/username/link", {
    headers: await login(request, first),
    data: { username: `reserved-${uniqueSuffix}`, password },
  });
  expect(response.status()).toBe(403);
});

test("unlinking removes the login but never the last one", async ({
  request,
}) => {
  const headers = await login(request, first);

  const unlinkSecond = await request.post("/auth/wasp/unlink", {
    headers,
    data: { method: "username", providerUserId: second },
  });
  expect(unlinkSecond.status()).toBe(200);

  const loginViaSecond = await request.post("/auth/wasp/username/login", {
    data: { username: second, password },
  });
  expect(loginViaSecond.status()).toBe(401);

  const unlinkLast = await request.post("/auth/wasp/unlink", {
    headers,
    data: { method: "username", providerUserId: first },
  });
  expect(unlinkLast.status()).toBe(409);
});

test("someone else's login cannot be unlinked", async ({ request }) => {
  const response = await request.post("/auth/wasp/unlink", {
    headers: await login(request, first),
    data: { method: "username", providerUserId: stranger },
  });
  expect(response.status()).toBe(404);
});

test("linking and unlinking through the package's client actions", async ({
  page,
}) => {
  const username = `erin-${uniqueSuffix}`;
  const linked = `erin-alt-${uniqueSuffix}`;

  await page.goto(`${WASP_CLIENT_URL}/login`);
  await page.getByRole("button", { name: "I need an account" }).click();
  await page.locator('input[name="username"]').fill(username);
  await page.locator('input[name="password"]').fill(password);
  await page.getByRole("button", { name: "Sign up" }).click();
  await expect(page.getByText("Signed in as")).toBeVisible();

  await page.goto(`${WASP_CLIENT_URL}/accounts`);
  await page.locator('input[name="link-username"]').fill(linked);
  await page.locator('input[name="link-password"]').fill(password);
  await page.getByRole("button", { name: "Add login" }).click();
  // `refreshUser` refetched the user: the new login shows without a reload.
  await expect(
    page.getByRole("button", { name: `Disconnect ${linked}` }),
  ).toBeVisible();

  await page.getByRole("button", { name: `Disconnect ${linked}` }).click();
  await expect(
    page.getByRole("button", { name: `Disconnect ${linked}` }),
  ).toHaveCount(0);
});
