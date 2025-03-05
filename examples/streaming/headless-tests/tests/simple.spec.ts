import { test, expect } from "@playwright/test";

test.describe("loads successfully", () => {
  test.describe.configure({ mode: "serial" });

  test("can see streamed content", async ({ page }) => {
    await page.goto("/");

    const finalText = `Hm, let me see... let's talk about number 1 and let's talk about number 2 and let's talk about number 3 and let's talk about number 4 and let's talk about number 5 and let's talk about number 6 and let's talk about number 7 and let's talk about number 8 and let's talk about number 9 and and finally about 10.`;
    await expect(page.locator("p")).toHaveText(finalText);
  });
});
