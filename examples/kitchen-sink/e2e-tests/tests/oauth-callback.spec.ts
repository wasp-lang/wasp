import { expect, test, type Page } from "@playwright/test";
import { WASP_SERVER_URL } from "../playwright.config";

const googleCallbackUrl = `${WASP_SERVER_URL}/auth/google/callback`;

test.describe("OAuth callback errors", () => {
  test("cancelling on the provider's consent screen shows a cancelled message", async ({
    page,
  }) => {
    const state = await startGoogleLogin(page);

    await page.goto(`${googleCallbackUrl}?error=access_denied&state=${state}`);

    await expect(page.locator("body")).toContainText(
      "Login with Google was cancelled or denied.",
    );
  });

  test("a callback without a started login shows an expired message", async ({
    page,
  }) => {
    await page.goto(`${googleCallbackUrl}?code=some-code&state=some-state`);

    await expect(page.locator("body")).toContainText(
      "Your login attempt with Google expired. Please try again.",
    );
  });

  test("a callback without the PKCE code verifier shows an expired message", async ({
    page,
  }) => {
    const state = await startGoogleLogin(page);
    await page.context().clearCookies({ name: "google_codeVerifier" });

    await page.goto(`${googleCallbackUrl}?code=some-code&state=${state}`);

    await expect(page.locator("body")).toContainText(
      "Your login attempt with Google expired. Please try again.",
    );
  });

  test("a callback without a code shows a generic message", async ({
    page,
  }) => {
    const state = await startGoogleLogin(page);

    await page.goto(`${googleCallbackUrl}?state=${state}`);

    await expect(page.locator("body")).toContainText(
      "Unable to log in with Google. Please try again later.",
    );
  });

  test("a provider error shows a generic message without the provider's description", async ({
    page,
  }) => {
    const state = await startGoogleLogin(page);

    await page.goto(
      `${googleCallbackUrl}?error=server_error&error_description=Provider+details&state=${state}`,
    );

    await expect(page.locator("body")).toContainText(
      "Unable to log in with Google. Please try again later.",
    );
    await expect(page.locator("body")).not.toContainText("Provider details");
  });
});

/**
 * Starts a Google login without following the redirect to Google, so the
 * server sets its OAuth cookies in the page's browser context.
 * Returns the `state` the server sent to Google.
 */
async function startGoogleLogin(page: Page): Promise<string> {
  const response = await page.request.get(
    `${WASP_SERVER_URL}/auth/google/login`,
    { maxRedirects: 0 },
  );

  const googleUrl = new URL(response.headers()["location"]);
  const state = googleUrl.searchParams.get("state");
  expect(state).not.toBeNull();
  return state!;
}
