import { expect, test } from "@playwright/test";

test.describe("Link component", () => {
  test("updates href when search prop changes", async ({ page }) => {
    await page.goto("/link-test");

    const searchLink = page.locator("#searchLink");
    await searchLink.waitFor();

    await expect(searchLink).toHaveAttribute("href", "/login?q=foo");

    await page.locator("#toggleSearch").click();

    await expect(searchLink).toHaveAttribute("href", "/login?q=bar");
  });

  test("updates href when hash prop changes", async ({ page }) => {
    await page.goto("/link-test");

    const hashLink = page.locator("#hashLink");
    await hashLink.waitFor();

    await expect(hashLink).toHaveAttribute("href", "/#top");

    await page.locator("#toggleHash").click();

    await expect(hashLink).toHaveAttribute("href", "/#bottom");
  });
});
