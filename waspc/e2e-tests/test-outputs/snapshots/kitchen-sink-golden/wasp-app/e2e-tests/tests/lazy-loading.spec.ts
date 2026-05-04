import { expect, test } from "@playwright/test";

test.describe("lazy loading routes", () => {
  test("eager page is loaded on unrelated pages", async ({ page }) => {
    await page.goto("/");

    const eagerLoaded = await page.evaluate(
      () => (window as any).__EAGER_PAGE_LOADED__,
    );
    expect(eagerLoaded).toBe(true);
  });

  test("lazy page is not loaded on unrelated pages", async ({ page }) => {
    await page.goto("/");

    const lazyLoaded = await page.evaluate(
      () => (window as any).__LAZY_PAGE_LOADED__,
    );
    expect(lazyLoaded).toBeUndefined();
  });

  test("lazy page is loaded when visiting it directly", async ({ page }) => {
    await page.goto("/lazy/yes");
    await expect(page.getByTestId("lazy-yes-page")).toBeAttached();

    const lazyLoaded = await page.evaluate(
      () => (window as any).__LAZY_PAGE_LOADED__,
    );
    expect(lazyLoaded).toBe(true);
  });

  test("eager page renders correctly when visited", async ({ page }) => {
    await page.goto("/lazy/no");
    await expect(page.getByTestId("lazy-no-page")).toBeAttached();
  });
});
