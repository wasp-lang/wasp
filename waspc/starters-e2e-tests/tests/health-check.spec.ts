import { expect, test } from "@playwright/test";

test("health check", async ({ page }) => {
  await page.goto("/");
  await expect(page.locator("body")).toBeVisible();
});
