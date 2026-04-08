import { expect, test, type ConsoleMessage, type Page } from "@playwright/test";

function collectConsoleMessages(page: Page): ConsoleMessage[] {
  const messages: ConsoleMessage[] = [];
  page.on("console", (msg) => {
    if (msg.type() === "error" || msg.type() === "warning") {
      messages.push(msg);
    }
  });
  return messages;
}

function hasHydrationWarning(messages: ConsoleMessage[]): boolean {
  return messages.some(
    (msg) =>
      msg.text().includes("Hydration") ||
      msg.text().includes("did not match") ||
      msg.text().includes("hydrat"),
  );
}

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

  test("prerender page has no hydration warnings", async ({ page }) => {
    const messages = collectConsoleMessages(page);

    await page.goto("/prerender");
    await expect(page.getByTestId("prerender-with-useisclient")).toHaveText(
      "This content is rendered on the client.",
    );

    expect(hasHydrationWarning(messages)).toBe(false);
  });

  test("hydration mismatch page triggers a hydration warning", async ({
    page,
  }) => {
    const messages = collectConsoleMessages(page);

    await page.goto("/hydration-mismatch");
    await expect(page.getByTestId("hydration-mismatch-content")).toBeVisible();

    expect(hasHydrationWarning(messages)).toBe(true);
  });
});
