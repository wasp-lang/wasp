import { expect, type Page, test } from "@playwright/test";
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
