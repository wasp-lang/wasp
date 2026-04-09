import { expect, Page, test } from "@playwright/test";

test.describe("prerender", () => {
  test("prerender page content is visible with no javascript", async ({
    browser,
  }) => {
    const context = await browser.newContext({ javaScriptEnabled: false });
    const page = await context.newPage();

    await page.goto("/prerender");

    await expect(page.getByTestId("render-location")).toHaveText("server");
  });

  test("prerender page hydrates on the client", async ({ page }) => {
    await page.goto("/prerender");

    await expect(page.getByTestId("render-location")).toHaveText("client");
  });
});

test.describe("hydration warnings", () => {
  test("prerender page has no hydration warnings", async ({ page }) => {
    using hydrationWarning = checkHydrationWarning(page);

    await page.goto("/prerender");

    await expect(page.getByTestId("render-location")).toHaveText("client");

    expect(hydrationWarning.hasHappened()).toBe(false);
  });

  test("hydration mismatch page triggers a hydration warning", async ({
    page,
  }) => {
    using hydrationWarning = checkHydrationWarning(page);

    await page.goto("/hydration-mismatch");

    await expect(page.getByTestId("render-location")).toHaveText("client");

    expect(hydrationWarning.hasHappened()).toBe(true);
  });
});

function checkHydrationWarning(page: Page) {
  let hasHappened = false;

  function listener(msg: Error) {
    hasHappened ||=
      // Unminified error message
      msg.message.includes("https://react.dev/link/hydration-mismatch") ||
      // Minified error message
      msg.message.includes("https://react.dev/errors/418");
  }

  page.on("pageerror", listener);

  return {
    hasHappened() {
      return hasHappened;
    },
    [Symbol.dispose]() {
      page.off("pageerror", listener);
    },
  };
}
