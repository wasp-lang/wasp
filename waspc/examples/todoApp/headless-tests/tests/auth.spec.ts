import { expect, test } from "@playwright/test";
import {
  generateRandomCredentials,
  performEmailVerification,
  submitLoginForm,
  submitSignupForm,
} from "./helpers";

test.describe("auth", () => {
  // We need the login test to run after the signup test
  test.describe.configure({ mode: "serial" });

  const { email, password } = generateRandomCredentials();

  test("social button renders", async ({ page }) => {
    await page.goto("/signup");

    await page.waitForSelector("text=Create a new account");

    await expect(
      page.locator("a[href='http://localhost:3001/auth/google/login']"),
    ).toBeVisible();
  });

  test("can sign up", async ({ page }) => {
    await submitSignupForm(page, {
      email,
      password,
    });

    await expect(page.locator("body")).toContainText(
      `You've signed up successfully! Check your email for the confirmation link.`,
    );

    await performEmailVerification(page, email);
  });

  test("can log in and create a task", async ({ page }) => {
    await submitLoginForm(page, {
      email,
      password: "12345678xxx",
    });

    await expect(page.locator("body")).toContainText("Invalid credentials");

    await submitLoginForm(page, {
      email,
      password,
    });

    await expect(page).toHaveURL("/");
  });
});
