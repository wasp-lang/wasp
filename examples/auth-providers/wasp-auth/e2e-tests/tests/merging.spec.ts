import { expect, test, type APIRequestContext } from "@playwright/test";

/**
 * Account merging inside Wasp's own auth. The safety properties matter more
 * than the happy path: a merge is only offered to someone who proved control
 * of the other account, the ticket only works for the account it was issued
 * to, and the app's `mergeUsers` moves the data before the account goes.
 */

const uniqueSuffix = `${Date.now()}-${Math.floor(Math.random() * 10000)}`;
const password = "password1234";
const otherPassword = "different5678";
const keeper = `frank-${uniqueSuffix}`;
const absorbed = `frank-old-${uniqueSuffix}`;
const bystander = `grace-${uniqueSuffix}`;

test.describe.configure({ mode: "serial" });

type Headers = { Authorization: string };

async function signup(
  request: APIRequestContext,
  username: string,
  pass = password,
) {
  const response = await request.post("/auth/wasp/username/signup", {
    data: { username, password: pass },
  });
  expect(response.status()).toBe(200);
}

async function login(
  request: APIRequestContext,
  username: string,
  pass = password,
): Promise<Headers> {
  const response = await request.post("/auth/wasp/username/login", {
    data: { username, password: pass },
  });
  expect(response.status()).toBe(200);
  const { credential } = (await response.json()) as { credential: string };
  return { Authorization: `Bearer ${credential}` };
}

async function userId(request: APIRequestContext, headers: Headers) {
  const response = await request.get("/auth/me", { headers });
  expect(response.status()).toBe(200);
  return ((await response.json()) as { json: { id: string } }).json.id;
}

async function taskDescriptions(request: APIRequestContext, headers: Headers) {
  const response = await request.post("/operations/get-my-tasks", {
    headers,
    data: { json: null },
  });
  expect(response.status()).toBe(200);
  const { json } = (await response.json()) as {
    json: Array<{ description: string }>;
  };
  return json.map((task) => task.description);
}

let mergeTicket: string;
let absorbedHeaders: Headers;

test("set up two accounts, the older one owning a task", async ({
  request,
}) => {
  await signup(request, keeper);
  await signup(request, absorbed, otherPassword);
  await signup(request, bystander);
  absorbedHeaders = await login(request, absorbed, otherPassword);
  const created = await request.post("/operations/create-task", {
    headers: absorbedHeaders,
    data: { json: { description: `task of ${absorbed}` } },
  });
  expect(created.status()).toBe(200);
});

test("naming another account without its password offers no merge", async ({
  request,
}) => {
  const response = await request.post("/auth/wasp/username/link", {
    headers: await login(request, keeper),
    data: { username: absorbed, password: "not-the-password1" },
  });
  expect(response.status()).toBe(409);
  const body = (await response.json()) as { data?: { mergeTicket?: string } };
  expect(body.data?.mergeTicket).toBeUndefined();
});

test("proving the other account's password offers a merge ticket", async ({
  request,
}) => {
  const response = await request.post("/auth/wasp/username/link", {
    headers: await login(request, keeper),
    data: { username: absorbed, password: otherPassword },
  });
  expect(response.status()).toBe(409);
  const body = (await response.json()) as {
    data: { reason: string; mergeTicket: string };
  };
  expect(body.data.reason).toBe("merge-required");
  mergeTicket = body.data.mergeTicket;
  expect(typeof mergeTicket).toBe("string");
});

test("a ticket cannot be redeemed by a different account", async ({
  request,
}) => {
  const response = await request.post("/auth/wasp/merge", {
    headers: await login(request, bystander),
    data: { mergeTicket },
  });
  expect(response.status()).toBe(403);
});

test("a forged ticket is rejected", async ({ request }) => {
  const response = await request.post("/auth/wasp/merge", {
    headers: await login(request, keeper),
    data: { mergeTicket: `${mergeTicket}x` },
  });
  expect(response.status()).toBe(400);
});

test("confirming merges the accounts: data moved, both logins reach the keeper", async ({
  request,
}) => {
  const keeperHeaders = await login(request, keeper);
  const keeperId = await userId(request, keeperHeaders);

  const merge = await request.post("/auth/wasp/merge", {
    headers: keeperHeaders,
    data: { mergeTicket },
  });
  expect(merge.status()).toBe(200);

  // The app's mergeUsers moved the task.
  expect(await taskDescriptions(request, keeperHeaders)).toContain(
    `task of ${absorbed}`,
  );
  // The absorbed account's login now signs into the surviving user.
  const viaAbsorbed = await login(request, absorbed, otherPassword);
  expect(await userId(request, viaAbsorbed)).toBe(keeperId);
});

test("the absorbed account's old credential stopped working", async ({
  request,
}) => {
  const response = await request.get("/auth/me", { headers: absorbedHeaders });
  expect(response.status()).toBe(401);
});

test("the spent ticket cannot merge again", async ({ request }) => {
  const response = await request.post("/auth/wasp/merge", {
    headers: await login(request, keeper),
    data: { mergeTicket },
  });
  expect(response.status()).not.toBe(200);
});
