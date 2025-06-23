import { expect, test } from "@playwright/test";
import { WASP_SERVER_PORT } from "../playwright.config";
import {
  generateRandomEmail,
  getEmailVerificationLink,
  isRunningInDevMode,
  submitLoginForm,
  submitSignupForm,
} from "./helpers";

test.describe("auth", () => {
  test("social button renders on signup page", async ({ page }) => {
    await page.goto("/signup");

    await expect(
      page.locator(
        `a[href='http://localhost:${WASP_SERVER_PORT}/auth/google/login']`,
      ),
    ).toBeVisible();
  });

  test.describe("signup and login", () => {
    const email = generateRandomEmail();
    const password = "12345678";

    // We need the login test to run after the signup test.
    test.describe.configure({ mode: "serial" });

    test("can sign up", async ({ page }) => {
      await submitSignupForm(page, {
        email,
        password,
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

      await submitLoginForm(page, {
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

      // Wait for the email to be sent
      await page.waitForTimeout(1000);

      const link = await getEmailVerificationLink(page, email);

      await page.goto(link);
      await expect(page.locator("body")).toContainText(
        "Your email has been verified",
      );
    });

    test("invalid credentials result in error message", async ({ page }) => {
      const invalidPassword = `${password}xxx`;
      await submitLoginForm(page, {
        email,
        password: invalidPassword,
      });

      await expect(page.locator("body")).toContainText("Invalid credentials");
    });

    test("can log in", async ({ page }) => {
      await submitLoginForm(page, {
        email,
        password,
      });

      await expect(page).toHaveURL("/");
    });
  });
});
