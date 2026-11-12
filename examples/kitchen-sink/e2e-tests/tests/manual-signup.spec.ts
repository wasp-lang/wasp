import { expect, test } from "@playwright/test";
import { performEmailVerification, performLogin, performSignup } from "./auth";
import { generateRandomEmail, isRunningInDeployedMode } from "./helpers";

test.describe("manual signup", () => {
  // These tests read emails. Dev mode uses the Dummy email sender, build mode
  // uses Mailcrab over SMTP.
  test.skip(
    isRunningInDeployedMode(),
    "Skipped in deployed mode (no Mailcrab)",
  );

  // We need the login test to run after the signup test.
  test.describe.configure({ mode: "serial" });

  const email = generateRandomEmail();
  const password = "12345678";

  test("can sign up ", async ({ page }) => {
    await performSignup(
      page,
      {
        email,
        password,
        address: "Some at least 10 letter address",
      },
      "manual-signup",
    );
    await expect(page.locator("body")).toContainText(
      `You've signed up successfully! Check your email for the confirmation link.`,
    );
  });

  // Unlike custom sign up, manual still follows the standard Wasp flow.
  test("can verify email", async ({ page }) => {
    await performEmailVerification(page, email);
  });

  test("can log in after email verification with manual sign up", async ({
    page,
  }) => {
    await performLogin(page, {
      email,
      password,
    });
    await expect(page).toHaveURL("/");

    await page.goto("/profile");
    await expect(
      page.getByTestId("user-profile").getByTestId("user-id"),
    ).toHaveText(email);
  });
});
