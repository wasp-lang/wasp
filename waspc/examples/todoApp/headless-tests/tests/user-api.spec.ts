import { expect, test } from "@playwright/test";
import {
  generateRandomCredentials,
  performEmailVerification,
  performSignup,
} from "./helpers";

test.describe("user API", () => {
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

  test("user API works on the client", async ({ page }) => {
    await page.goto("/login");

    await page.waitForSelector("text=Log in to your account");

    await page.locator("input[type='email']").fill(email);
    await page.locator("input[type='password']").fill(password);
    await page.getByRole("button", { name: "Log in" }).click();

    await expect(page).toHaveURL("/");

    await page.goto("/profile");

    await expect(page).toHaveURL("/profile");

    await expect(
      page.getByTestId("user-profile").getByTestId("user-id"),
    ).toContainText(email);
    await expect(
      page.getByTestId("user-profile").getByTestId("email-status"),
    ).toContainText("Verified");
  });
});
