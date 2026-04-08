import { expect, test, type ConsoleMessage, type Page } from "@playwright/test";

function collectConsoleErrors(page: Page): ConsoleMessage[] {
  const errors: ConsoleMessage[] = [];
  page.on("console", (msg) => {
    if (msg.type() === "error") {
      errors.push(msg);
    }
  });
  return errors;
}

function hasHydrationWarning(errors: ConsoleMessage[]): boolean {
  return errors.some(
    (msg) =>
      msg.text().includes("Hydration") ||
      msg.text().includes("did not match") ||
      msg.text().includes("hydrating"),
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
    const errors = collectConsoleErrors(page);

    await page.goto("/prerender");
    await expect(page.getByTestId("prerender-with-useisclient")).toHaveText(
      "This content is rendered on the client.",
    );

    expect(hasHydrationWarning(errors)).toBe(false);
  });

  test("hydration mismatch page triggers a hydration warning", async ({
    page,
  }) => {
    const errors = collectConsoleErrors(page);

    await page.goto("/hydration-mismatch");
    await expect(page.getByTestId("hydration-mismatch-content")).toBeVisible();

    expect(hasHydrationWarning(errors)).toBe(true);
  });
});
