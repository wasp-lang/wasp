import { expect, test } from "@playwright/test";
import {
  generateRandomEmail,
  isRunningInDevMode,
  performEmailVerification,
  performLogin,
  submitLoginForm,
  submitSignupForm,
} from "./helpers";

test.describe("auth hooks", () => {
  /*
    We set up the "before signup hook" to throw an error for a specific email address.
  */
  test("before signup hook works", async ({ page }) => {
    const emailThatThrowsError = "notallowed@email.com";
    const password = "12345678";

    await submitSignupForm(page, {
      email: emailThatThrowsError,
      password,
    });

    await expect(page.locator("body")).toContainText(
      "On Before Signup Hook disallows this email.",
    );
  });

  /*
    We set up the "after signup hook" to set a value in the user object.
    We also set up the "after login hook" to set a value in the user object.
  */
  test("after signup and after login hooks work", async ({ page }) => {
    const email = generateRandomEmail();
    const password = "12345678";

    await submitSignupForm(page, {
      email,
      password,
    });

    await performEmailVerification(page, email);

    await performLogin(page, {
      email,
      password,
    });

    await page.goto("/profile");

    await expect(
      page.getByTestId("hook-status-onAfterSignup").getByTestId("status"),
    ).toHaveText("Called");

    await expect(
      page.getByTestId("hook-status-onAfterLogin").getByTestId("status"),
    ).toHaveText("Called");

    const expectedEmailVerificationStatus = isRunningInDevMode()
      ? "Not Called"
      : "Called";
    await expect(
      page
        .getByTestId("hook-status-onAfterEmailVerified")
        .getByTestId("status"),
    ).toHaveText(expectedEmailVerificationStatus);
  });

  /*
    We set up the "before login hook" to throw an error for a specific email address.
  */
  test("before login hook works", async ({ page }) => {
    const emailThatThrowsError = "cantlogin@email.com";
    const password = "12345678";

    await submitSignupForm(page, {
      email: emailThatThrowsError,
      password,
    });

    await performEmailVerification(page, emailThatThrowsError);

    await submitLoginForm(page, {
      email: emailThatThrowsError,
      password,
    });

    await expect(page.locator("body")).toContainText(
      "On Before Login Hook disallows this email.",
    );
  });
});
