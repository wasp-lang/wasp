import { expect, test } from "@playwright/test";
import { performEmailVerification, performLogin, performSignup } from "./auth";
import {
  generateRandomEmail,
  isRunningInDeployedMode,
  isRunningInDevMode,
} from "./helpers";

test.describe("auth", () => {
  test("social button renders on signup page", async ({ page }) => {
    await page.goto("/signup");

    await expect(
      // The href is relative when the client is served same-origin (dev)
      // and absolute when REACT_APP_API_URL is set (deployed mode).
      page.locator(`a[href$='/auth/google/login']`),
    ).toBeVisible();
  });

  test.describe("signup and login", () => {
    // These tests require Mailcrab for email verification.
    test.skip(
      isRunningInDeployedMode(),
      "Skipped in deployed mode (no Mailcrab)",
    );

    // We need the login test to run after the signup test.
    test.describe.configure({ mode: "serial" });

    const email = generateRandomEmail();
    const password = "12345678";

    test("failing custom signup fields requirements results in error message", async ({
      page,
    }) => {
      await performSignup(page, {
        email,
        password,
        address: "TooShort",
      });

      await expect(page.locator("body")).toContainText(
        `Address must be at least 10 characters long.`,
      );
    });

    test("can sign up", async ({ page }) => {
      await performSignup(page, {
        email,
        password,
        address: "Some at least 10 letter address",
      });

      await expect(page.locator("body")).toContainText(
        `You've signed up successfully! Check your email for the confirmation link.`,
      );
    });

    test("unconfirmed email results in error message", async ({ page }) => {
      if (isRunningInDevMode()) {
        // Skip this test in dev mode, as email confirmation is not required.
        test.skip();
      }

      await performLogin(page, {
        email,
        password,
      });

      await expect(page.locator("body")).toContainText("Invalid credentials");
    });

    test("can verify email", async ({ page }) => {
      if (isRunningInDevMode()) {
        // Skip this test in dev mode, as email confirmation is not required.
        test.skip();
      }

      await performEmailVerification(page, email);
    });

    test("invalid credentials result in error message", async ({ page }) => {
      const invalidPassword = `${password}xxx`;

      await performLogin(page, {
        email,
        password: invalidPassword,
      });
      await expect(page.locator("body")).toContainText("Invalid credentials");
    });

    test("can log in", async ({ page }) => {
      await performLogin(page, {
        email,
        password,
      });
      await expect(page).toHaveURL("/");
    });
  });
});
