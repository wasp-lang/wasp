import type { Page } from "@playwright/test";
import { SERVER_URL } from "../env";

export type TestUser = { userId: string; username: string; password: string };

/** Creates an already-verified user through the test-only API (no signup). */
export async function createVerifiedUser(): Promise<TestUser> {
  const res = await fetch(`${SERVER_URL}/__test/users`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({
      username: `user-${Date.now()}-${Math.random().toString(36).slice(2, 8)}`,
    }),
  });
  if (!res.ok)
    throw new Error(
      `createVerifiedUser failed: ${res.status} ${await res.text()}`,
    );
  return (await res.json()) as TestUser;
}

/** Logs in through the real login page. */
export async function logIn(page: Page, user: TestUser): Promise<void> {
  await page.goto("/login");
  await page.locator('input[name="username"]').fill(user.username);
  await page.locator('input[name="password"]').fill(user.password);
  await page.locator('button[type="submit"]').click();
  await page.getByRole("button", { name: "Log out" }).waitFor();
  await page.waitForURL(/\/links$/);
  await page.waitForLoadState("networkidle");
}

/** Navigates and waits until the SPA has finished loading data, so forms don't re-render mid-fill. */
export async function gotoReady(page: Page, path: string): Promise<void> {
  await page.goto(path);
  await page.waitForLoadState("networkidle");
}
