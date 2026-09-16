import { expect, test, type Page } from "@playwright/test";
import { WASP_SERVER_URL } from "../playwright.config";
import { performLogin, setupTestUser } from "./auth";
import { isRunningInDeployedMode, isRunningInDevMode } from "./helpers";
import { getMailCrabPasswordResetLink } from "./mailcrab";

test.describe("password reset", () => {
  // The flow needs Mailcrab to receive the reset email, which wasp-app-runner
  // starts only in build mode -- same constraint as the email verification
  // tests.
  test.skip(
    isRunningInDeployedMode() || isRunningInDevMode(),
    "Runs only in build mode (needs Mailcrab)",
  );

  // The whole flow is one story: request the email, follow the link, set the
  // new password, then prove what it changed.
  test.describe.configure({ mode: "serial" });

  const credentials = setupTestUser();
  const newPassword = "87654321-new";

  // A session established BEFORE the reset, to prove the reset revokes it.
  let preResetSessionId: string | null = null;

  test("can request a password reset email", async ({ page }) => {
    await performLogin(page, credentials);
    await expect(page).toHaveURL("/");
    preResetSessionId = await getStoredSessionId(page);
    expect(preResetSessionId).not.toBeNull();

    await page.goto("/request-password-reset");
    await page.locator("input[type='email']").fill(credentials.email);
    await page
      .getByRole("button", { name: "Send password reset email" })
      .click();

    await expect(page.locator("body")).toContainText(
      "Check your email for a password reset link.",
    );
  });

  test("can reset the password via the emailed link", async ({ page }) => {
    // Wait for the email to be sent.
    await page.waitForTimeout(1000);
    const link = await getMailCrabPasswordResetLink(page, credentials.email);

    await page.goto(link);
    await page.waitForSelector("text=New password");
    const passwordInputs = page.locator("input[type='password']");
    await passwordInputs.nth(0).fill(newPassword);
    await passwordInputs.nth(1).fill(newPassword);
    await page.getByRole("button", { name: "Reset password" }).click();

    await expect(page.locator("body")).toContainText(
      "Your password has been reset.",
    );
  });

  test("old password no longer works and pre-reset session is revoked", async ({
    page,
  }) => {
    await performLogin(page, credentials);
    await expect(page.locator("body")).toContainText("Invalid credentials");

    // Changing the password invalidates every existing session server-side.
    const response = await page.request.get(`${WASP_SERVER_URL}/auth/me`, {
      headers: { Authorization: `Bearer ${preResetSessionId}` },
    });
    expect(response.status()).toBe(401);
  });

  test("new password works", async ({ page }) => {
    await performLogin(page, {
      email: credentials.email,
      password: newPassword,
    });
    await expect(page).toHaveURL("/");
  });
});

async function getStoredSessionId(page: Page): Promise<string | null> {
  const raw = await page.evaluate(() => localStorage.getItem("wasp:sessionId"));
  return raw === null ? null : (JSON.parse(raw) as string);
}
