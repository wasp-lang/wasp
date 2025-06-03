import { expect, test } from "@playwright/test";
import {
  generateRandomCredentials,
  performEmailVerification,
  performSignup,
} from "./helpers";

test.describe("custom APIs", () => {
  const { email, password } = generateRandomCredentials();

  test.describe.configure({ mode: "serial" });

  test.beforeAll(async ({ browser }) => {
    const page = await browser.newPage();

    await performSignup(page, {
      email,
      password,
    });

    await expect(page.locator("body")).toContainText(
      `You've signed up successfully! Check your email for the confirmation link.`,
    );

    await performEmailVerification(page, email);
  });

  test("unauthenticated APIs work", async ({ page }) => {
    await page.goto("/apis");

    await expect(page).toHaveURL("/apis");

    // Authenticated API should return an error
    await expect(
      page.getByTestId("authenticated-api").getByTestId("error"),
    ).toBeVisible();
    await expect(
      page.getByTestId("authenticated-api").getByTestId("data"),
    ).not.toBeVisible();

    // Unauthenticated API should return data
    await expect(
      page.getByTestId("unauthenticated-api").getByTestId("error"),
    ).not.toBeVisible();
    await expect(
      page.getByTestId("unauthenticated-api").getByTestId("data"),
    ).toContainText("Hello, stranger!");
  });

  test("authenticated APIs work", async ({ page }) => {
    await page.goto("/login");

    await page.waitForSelector("text=Log in to your account");

    await page.locator("input[type='email']").fill(email);
    await page.locator("input[type='password']").fill(password);
    await page.getByRole("button", { name: "Log in" }).click();

    await expect(page).toHaveURL("/");

    await page.goto("/apis");

    await expect(page).toHaveURL("/apis");

    // Authenticated API should return data
    await expect(
      page.getByTestId("authenticated-api").getByTestId("error"),
    ).not.toBeVisible();
    await expect(
      page.getByTestId("authenticated-api").getByTestId("data"),
    ).toContainText(`Hello, ${email}!`);

    // Unauthenticated API should return data
    await expect(
      page.getByTestId("unauthenticated-api").getByTestId("error"),
    ).not.toBeVisible();
    await expect(
      page.getByTestId("unauthenticated-api").getByTestId("data"),
    ).toContainText("Hello, stranger!");
  });
});
