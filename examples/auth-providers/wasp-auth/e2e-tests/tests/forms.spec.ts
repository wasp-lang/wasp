import { expect, test } from "@playwright/test";
import { WASP_CLIENT_URL } from "../playwright.config";

/**
 * The client half: the package's forms, instantiated through the client
 * adapter seam, post to the package's routes and adopt the minted session
 * through the provider-bound sink, so the auth gate lets the user through.
 */

const uniqueSuffix = `${Date.now()}-${Math.floor(Math.random() * 10000)}`;
const username = `bob-${uniqueSuffix}`;
const password = "password1234";

test("signing up and logging in through the package's forms", async ({
  page,
}) => {
  await page.goto(`${WASP_CLIENT_URL}/login`);

  // Username signup logs the user in straight away and redirects.
  await page.getByRole("button", { name: "I need an account" }).click();
  await page.locator('input[name="username"]').fill(username);
  await page.locator('input[name="password"]').fill(password);
  await page.getByRole("button", { name: "Sign up" }).click();
  await expect(page).toHaveURL(`${WASP_CLIENT_URL}/`);
  await expect(page.getByText("Signed in as")).toBeVisible();

  await page.getByRole("button", { name: "Log out" }).click();
  await expect(page).toHaveURL(`${WASP_CLIENT_URL}/login`);

  await page.locator('input[name="username"]').fill(username);
  await page.locator('input[name="password"]').fill(password);
  await page.getByRole("button", { name: "Log in" }).click();
  await expect(page).toHaveURL(`${WASP_CLIENT_URL}/`);
  await expect(page.getByText("Signed in as")).toBeVisible();
});
