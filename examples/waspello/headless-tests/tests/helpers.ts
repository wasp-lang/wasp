import type { Page } from "@playwright/test";

export async function performSignup(
  page: Page,
  { username, password }: { username: string; password: string },
) {
  await page.goto("/signup");

  await page.waitForSelector("text=Sign up for your account");

  await page.locator("input[placeholder='Enter email address']").fill(username);
  await page.locator("input[type='password']").fill(password);
  await page.locator('input[type="submit"]').click();
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

  await page.waitForSelector("text=Log in with your account");

  await page.locator("input[placeholder='Enter email address']").fill(username);
  await page.locator("input[type='password']").fill(password);
  await page.locator('input[type="submit"]').click();
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
