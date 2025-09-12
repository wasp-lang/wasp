import { expect, test } from "@playwright/test";

test.describe("loads successfully", () => {
  test.describe.configure({ mode: "serial" });

  test("can see stats", async ({ page }) => {
    await page.goto("/");
    // The stats don't load immediately
    await page.waitForTimeout(5000);
    await page.goto("/");

    // Wait for the dashboard to load
    await page.waitForSelector(".dashboard-item");
    await expect(page.locator(".dashboard-item")).toHaveCount(5);
  });
});
