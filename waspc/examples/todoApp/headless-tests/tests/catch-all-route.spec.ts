import { test, expect } from "@playwright/test";

test.describe("catch all route + oauth route", () => {
  test.describe.configure({ mode: "serial" });

  test("catch all route renders for unknown route", async ({ page }) => {
    await page.goto("/unknown-route");

    await page.waitForSelector("text=Not found");

    await expect(page.locator("body")).toContainText(
      `We couldn't find anything at the /unknown-route location.`
    );
  });

  // We wanted to prevent the user defined routes from overriding
  // the OAuth callback route. Details: https://github.com/wasp-lang/wasp/issues/2029
  test("oauth callback route renders at /ouath/callback", async ({ page }) => {
    await page.goto("/oauth/callback?error=example+error");

    await page.waitForSelector("text=example error");

    await expect(page.locator("body")).toContainText("example error");
  });
});
