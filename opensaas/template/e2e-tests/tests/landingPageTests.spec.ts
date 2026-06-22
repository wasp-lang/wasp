import { expect, test } from "@playwright/test";

test.describe("general landing page tests", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/");
  });

  test("has title", async ({ page }) => {
    await expect(page).toHaveTitle(/SaaS/);
  });

  test("get started link", async ({ page }) => {
    await page.getByRole("link", { name: "Get started" }).click();
    await page.waitForURL("**/signup");
  });

  test("headings", async ({ page }) => {
    await expect(
      page.getByRole("heading", { name: "Frequently asked questions" }),
    ).toBeVisible();
    await expect(
      page.getByRole("heading", { name: "Some cool words" }),
    ).toBeVisible();
  });
});

test.describe("cookie consent tests", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto("/");
  });

  test("cookie consent banner rejection does not set cc_cookie", async ({
    context,
    page,
  }) => {
    await page.$$('button:has-text("Reject all")');
    await page.click('button:has-text("Reject all")');

    const cookies = await context.cookies();
    const consentCookie = cookies.find((c) => c.name === "cc_cookie");
    const cookieObject = JSON.parse(decodeURIComponent(consentCookie.value));
    expect(cookieObject.categories.includes("analytics")).toBeFalsy();
  });

  test("cookie consent banner acceptance sets cc_cookie and _ga cookies", async ({
    context,
    page,
  }) => {
    await page.$$('button:has-text("Accept all")');
    await page.click('button:has-text("Accept all")');

    const cookies = await context.cookies();
    const consentCookie = cookies.find((c) => c.name === "cc_cookie");
    const cookieObject = JSON.parse(decodeURIComponent(consentCookie.value));
    // Check that the Cookie Consent cookie is set. This should happen immediately, and then the GA cookies will get set after it, dynamically.
    expect(cookieObject.categories.includes("analytics")).toBeTruthy();

    // GA cookies (_ga and _ga_<GA_ANALYTICS_ID>) are loaded asynchronously
    // after consent. Poll until both are present, allowing extra time for slow CI.
    await expect
      .poll(
        async () => {
          const cookies = await context.cookies();
          return cookies.filter((c) => c.name.startsWith("_ga")).length;
        },
        { timeout: 15000, intervals: [200, 500, 1000] },
      )
      .toBe(2);
  });
});
