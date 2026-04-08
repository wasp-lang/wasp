import { expect, test } from "@playwright/test";

test.describe("prerender", () => {
  test("prerender page renders with correct content", async ({ page }) => {
    await page.goto("/prerender");

    await expect(page.getByTestId("prerender-route")).toContainText(
      "prerender: true",
    );
  });

  test("prerender page hydrates on the client", async ({ page }) => {
    await page.goto("/prerender");

    await expect(page.getByTestId("prerender-with-useisclient")).toHaveText(
      "This content is rendered on the client.",
    );
  });
});
