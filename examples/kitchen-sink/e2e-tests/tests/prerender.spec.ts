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

  test("listed instances of a dynamic route are prerendered with no javascript", async ({
    browser,
  }) => {
    const context = await browser.newContext({ javaScriptEnabled: false });
    const page = await context.newPage();

    await page.goto("/prerender-instances/alice");

    await expect(page.getByTestId("render-location")).toHaveText("server");
    await expect(page.getByTestId("slug")).toHaveText("alice");
  });

  test("prerendered dynamic-route instance hydrates on the client", async ({
    page,
  }) => {
    await page.goto("/prerender-instances/bob");

    await expect(page.getByTestId("render-location")).toHaveText("client");
    await expect(page.getByTestId("slug")).toHaveText("bob");
  });

  test("an unlisted instance of the dynamic route is not prerendered", async ({
    browser,
  }) => {
    const context = await browser.newContext({ javaScriptEnabled: false });
    const page = await context.newPage();

    // "charlie" is not in the route's prerender list, so it is served as the
    // SPA shell instead of prerendered HTML. With no JavaScript, the page never
    // renders, so its content is absent.
    await page.goto("/prerender-instances/charlie");

    await expect(page.getByTestId("render-location")).toHaveCount(0);
  });

  test("an unlisted instance still renders on the client", async ({ page }) => {
    await page.goto("/prerender-instances/charlie");

    await expect(page.getByTestId("render-location")).toHaveText("client");
    await expect(page.getByTestId("slug")).toHaveText("charlie");
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
