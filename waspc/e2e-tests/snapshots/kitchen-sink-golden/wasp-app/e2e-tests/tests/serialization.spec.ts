import { expect, test } from "@playwright/test";

test.describe("Serialization test", () => {
  test("serialization of superjson-supported types works", async ({ page }) => {
    await page.goto("/serialization");

    const serializedObjectsEl = page.locator("#serializedObjects");
    await serializedObjectsEl.waitFor();
    await expect(serializedObjectsEl).toContainText(
      "All serialized objects are of the expected types.",
    );
  });
});
