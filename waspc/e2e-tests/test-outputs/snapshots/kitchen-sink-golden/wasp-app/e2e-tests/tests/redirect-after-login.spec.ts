import { expect, test, type Page } from "@playwright/test";
import { performLogin, setupTestUser } from "./auth";

// These tests rely on the app's auth config using
// `onAuthSucceededRedirect: redirectToOriginalRoute()`, and on the
// `/profile` page requiring auth.
test.describe("redirect after login", () => {
  const credentials = setupTestUser();

  test("redirects back to the originally requested URL after login", async ({
    page,
  }) => {
    await page.goto("/profile?key=value");
    await expect(page).toHaveURL("/login");

    await submitLoginForm(page, credentials);

    await expect(page).toHaveURL("/profile?key=value");
  });

  test("preserves the original URL while the user moves between auth pages", async ({
    page,
  }) => {
    await page.goto("/profile?key=value");
    await expect(page).toHaveURL("/login");

    // Wander off to the signup page and come back before logging in.
    await page.goto("/signup");
    await page.goto("/login");

    await submitLoginForm(page, credentials);

    await expect(page).toHaveURL("/profile?key=value");
  });

  test("falls back to the fixed route when logging in directly", async ({
    page,
  }) => {
    await performLogin(page, credentials);
    await expect(page).toHaveURL("/");
  });

  test("uses the original URL only once", async ({ page }) => {
    await page.goto("/profile?key=value");
    await expect(page).toHaveURL("/login");

    await submitLoginForm(page, credentials);
    await expect(page).toHaveURL("/profile?key=value");

    // Sign out from a public page — signing out while on a protected page
    // would immediately save that page as the original route again.
    await page.goto("/");
    await page.getByRole("button", { name: "Sign Out" }).click();

    await page.goto("/login");
    await submitLoginForm(page, credentials);
    await expect(page).toHaveURL("/");
  });
});

async function submitLoginForm(
  page: Page,
  credentials: { email: string; password: string },
) {
  await page.waitForSelector("text=Log in to your account");
  await page.locator("input[type='email']").fill(credentials.email);
  await page.locator("input[type='password']").fill(credentials.password);
  await page.getByRole("button", { name: "Log in" }).click();
}
