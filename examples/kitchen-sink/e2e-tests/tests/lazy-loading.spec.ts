import { expect, test } from "@playwright/test";

test.describe("lazy loading routes", () => {
  test("eager page (lazy: false) module is loaded on unrelated pages", async ({
    page,
  }) => {
    await page.goto("/");

    const eagerLoaded = await page.evaluate(
      () => (window as any).__EAGER_PAGE_LOADED__,
    );
    expect(eagerLoaded).toBe(true);
  });

  test("lazy page (lazy: true) module is NOT loaded on unrelated pages", async ({
    page,
  }) => {
    await page.goto("/");

    const lazyLoaded = await page.evaluate(
      () => (window as any).__LAZY_PAGE_LOADED__,
    );
    expect(lazyLoaded).toBeUndefined();
  });

  test("lazy page module IS loaded when visiting it directly", async ({
    page,
  }) => {
    await page.goto("/lazy/yes");

    const lazyLoaded = await page.evaluate(
      () => (window as any).__LAZY_PAGE_LOADED__,
    );
    expect(lazyLoaded).toBe(true);

    await expect(page.getByTestId("lazy-yes-page")).toHaveText(
      "Lazy page (lazy: true)",
    );
  });

  test("eager page renders correctly when visited", async ({ page }) => {
    await page.goto("/lazy/no");
    await expect(page.getByTestId("lazy-no-page")).toHaveText(
      "Eager page (lazy: false)",
    );
  });
});
