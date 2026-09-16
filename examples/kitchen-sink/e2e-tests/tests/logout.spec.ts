import { expect, test } from "@playwright/test";
import { WASP_SERVER_URL } from "../playwright.config";
import { performLogin, setupTestUser } from "./auth";
import { isRunningInDeployedMode } from "./helpers";

test.describe("logout", () => {
  // setupTestUser needs Mailcrab for email verification.
  test.skip(
    isRunningInDeployedMode(),
    "Skipped in deployed mode (no Mailcrab)",
  );

  const credentials = setupTestUser();

  test("logging out revokes the session server-side", async ({ page }) => {
    await performLogin(page, credentials);
    await expect(page).toHaveURL("/");

    // Capture the session the client holds so we can prove the server revoked
    // it (rather than the client merely dropping it).
    const sessionId = await page.evaluate(() =>
      localStorage.getItem("wasp:sessionId"),
    );
    expect(sessionId).not.toBeNull();

    await page.getByRole("button", { name: "Sign Out" }).click();
    await expect(
      page.getByRole("button", { name: "Sign Out" }),
    ).not.toBeVisible();

    // An auth-required page bounces to login...
    await page.goto("/profile");
    await expect(page).toHaveURL("/login");

    // ...and the old session is dead on the server, not just gone locally.
    const response = await page.request.get(`${WASP_SERVER_URL}/auth/me`, {
      headers: { Authorization: `Bearer ${JSON.parse(sessionId!)}` },
    });
    expect(response.status()).toBe(401);
  });
});
