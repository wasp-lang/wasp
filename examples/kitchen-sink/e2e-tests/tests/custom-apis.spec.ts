import { expect, type Page, test } from "@playwright/test";
import { WASP_SERVER_URL } from "../playwright.config";
import { performLogin, setupTestUser } from "./auth";

test.describe("custom APIs", () => {
  const credentials = setupTestUser();

  test("unauthenticated APIs work", async ({ page }) => {
    await page.goto("/apis");

    await expect(page).toHaveURL("/apis");

    await expectApiCallToError(page, {
      testId: "authenticated-api",
    });

    await expectApiCallToSucceed(page, {
      testId: "unauthenticated-api",
      expectedData: "Hello, stranger!",
    });
  });

  test("authenticated APIs work", async ({ page }) => {
    await performLogin(page, credentials);
    await expect(page).toHaveURL("/");

    await page.goto("/apis");

    await expect(page).toHaveURL("/apis");

    await expectApiCallToSucceed(page, {
      testId: "authenticated-api",
      expectedData: `Hello, ${credentials.email}!`,
    });

    await expectApiCallToSucceed(page, {
      testId: "unauthenticated-api",
      expectedData: "Hello, stranger!",
    });
  });

  test("APIs that ignore the server base path are served at the origin root", async ({
    request,
  }) => {
    const response = await request.get(`${WASP_SERVER_URL}/outside-base-path`);
    expect(response.status()).toBe(200);
    expect(await response.json()).toEqual({ ok: true });

    const responseUnderBasePath = await request.get(
      `${WASP_SERVER_URL}/api/outside-base-path`,
    );
    expect(responseUnderBasePath.status()).not.toBe(200);
  });
});

async function expectApiCallToError(
  page: Page,
  {
    testId,
  }: {
    testId: string;
  },
) {
  await expect(page.getByTestId(testId).getByTestId("error")).toBeVisible();
  await expect(page.getByTestId(testId).getByTestId("data")).not.toBeVisible();
}

async function expectApiCallToSucceed(
  page: Page,
  {
    testId,
    expectedData,
  }: {
    testId: string;
    expectedData: string;
  },
) {
  await expect(page.getByTestId(testId).getByTestId("error")).not.toBeVisible();
  await expect(page.getByTestId(testId).getByTestId("data")).toContainText(
    expectedData,
  );
}
