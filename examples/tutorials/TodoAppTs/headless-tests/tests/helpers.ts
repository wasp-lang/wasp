import type { Page } from "@playwright/test";

export async function performSignup(
  page: Page,
  { username, password }: { username: string; password: string },
) {
  await page.goto("/signup");

  await page.waitForSelector("text=Create a new account");

  await page.locator("input[name='username']").fill(username);
  await page.locator("input[type='password']").fill(password);
  await page.locator("button").click();
}

export async function performLogin(
  page: Page,
  {
    username,
    password,
  }: {
    username: string;
    password: string;
  },
) {
  await page.goto("/login");

  await page.waitForSelector("text=Log in to your account");

  await page.locator("input[name='username']").fill(username);
  await page.locator("input[type='password']").fill(password);
  await page.getByRole("button", { name: "Log in" }).click();
}

export function generateRandomCredentials(): {
  username: string;
  password: string;
} {
  return {
    username: `test${Math.random().toString(36).substring(7)}`,
    password: "12345678",
  };
}
