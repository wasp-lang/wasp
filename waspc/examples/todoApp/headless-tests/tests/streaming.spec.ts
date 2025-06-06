import { expect, test } from "@playwright/test";

test.describe("streaming", () => {
  test("text streams over time", async ({ page }) => {
    await page.goto("/streaming");

    // Verify initial text appears first
    await expect(page.locator("p")).toContainText("Hm, let me see...");

    // Wait for part of the text to appear (after 1-2 chunks)
    await expect(page.locator("p")).toContainText("let's talk about number 1");

    // Store the current text length
    const textLength1 = await page
      .locator("p")
      .evaluate((el) => el.textContent?.length ?? 0);

    // Wait a short period - enough time for at least one more chunk to arrive
    await page.waitForTimeout(700);

    // Check that the text has grown longer
    const textLength2 = await page
      .locator("p")
      .evaluate((el) => el.textContent?.length ?? 0);
    expect(textLength2).toBeGreaterThan(textLength1);

    // Verify some middle content eventually appears
    await expect(page.locator("p")).toContainText("let's talk about number 5");

    // Verify the complete text matches what we expect
    const finalText = `Hm, let me see... let's talk about number 1 and let's talk about number 2 and let's talk about number 3 and let's talk about number 4 and let's talk about number 5 and let's talk about number 6 and let's talk about number 7 and let's talk about number 8 and let's talk about number 9 and and finally about 10.`;
    await expect(page.locator("p")).toHaveText(finalText);
  });
});
