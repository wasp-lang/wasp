import { expect, test } from "@playwright/test";
import { SERVER_URL } from "./env";
import { gotoReady } from "./fixtures/users";

test("the docs pages search and render Markdown", async ({ page }) => {
  await gotoReady(page, "/docs");
  await page.getByLabel("Search docs").fill("how are clicks counted");
  await page
    .getByRole("link", { name: /How clicks are counted/ })
    .first()
    .click();
  await expect(
    page.getByRole("heading", { level: 1, name: "How clicks are counted" }),
  ).toBeVisible();
  await expect(
    page.getByRole("listitem").filter({ hasText: "Every redirect counts" }),
  ).toBeVisible();
  await gotoReady(page, "/docs/connecting-ai-clients");
  await expect(page.locator("pre code")).toHaveText(`${SERVER_URL}/mcp`);
});
