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

    // Wait for React to hydrate and re-render (the text changes from
    // "server" to "client" once hydration completes).
    await expect(page.getByTestId("render-location")).toHaveText("client");

    expect(hydrationWarning.hasHappened()).toBe(true);
  });
});

function checkHydrationWarning(page: Page) {
  let hasHappened = false;

  function listener(msg: Error) {
    hasHappened ||= msg.message.includes(
      "https://react.dev/link/hydration-mismatch",
    );
  }

  page.on("pageerror", listener);

  return {
    hasHappened() {
      return hasHappened;
    },
    [// @ts-expect-error Symbol.dispose is not yet in the TypeScript lib
    Symbol.dispose]() {
      page.off("pageerror", listener);
    },
  };
}
