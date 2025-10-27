import { expect, test } from "@playwright/test";

test.describe("loads successfully", () => {
  test.describe.configure({ mode: "serial" });

  test("shows the search bar", async ({ page }) => {
    await page.goto("/");
    await page.waitForSelector("input[type='search']");
    await expect(page.locator("input[type='search']")).toBeVisible();
  });
});
