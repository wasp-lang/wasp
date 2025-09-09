import { expect, test } from "@playwright/test";
import {
  generateRandomCredentials,
  performLogin,
  performSignup,
} from "./helpers";

test.describe("auth and cast a vote", () => {
  const { username, password } = generateRandomCredentials();

  test.describe.configure({ mode: "serial" });

  test("can sign up", async ({ page }) => {
    await performSignup(page, {
      username,
      password,
    });

    await expect(page).toHaveURL("/");

    await page.locator(".user-dropdown").click();
    await page.getByText("Sign out").click();

    await expect(page).toHaveURL("/login");
  });

  test("can log in and cast a vote", async ({ page }) => {
    await performLogin(page, {
      username,
      password: "12345678xxx",
    });

    await expect(page.locator("body")).toContainText("Invalid credentials");

    await performLogin(page, {
      username,
      password,
    });

    await expect(page).toHaveURL("/");

    await page.locator("button:has-text('Vote')").first().click();

    const card = page.locator(".card").first();
    await expect(
      card.locator(".username").filter({ hasText: username }),
    ).toBeVisible();
    await expect(card.locator("button")).toHaveText("Voted");
    await expect(card.locator("button")).toBeDisabled();
    await expect(card.locator(".votes-count")).not.toContainText("0 / 0");
  });
});
