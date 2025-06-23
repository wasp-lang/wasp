import { expect, test } from "@playwright/test";
import { generateRandomEmail, performLogin } from "./helpers";

test.describe("custom signup", () => {
  // We need the login test to run after the signup test.
  test.describe.configure({ mode: "serial" });

  const email = generateRandomEmail();
  const password = "12345678";

  test("can sign up", async ({ page }) => {
    await page.goto("/custom-signup");

    await expect(page.getByTestId("custom-signup-form")).toBeVisible();

    await page.locator("input[type='email']").fill(email);
    await page.locator("input[type='password']").fill(password);
    await page.locator("input[name='address']").fill("Dummy address");
    await page.locator('button[type="submit"]').click();

    await expect(
      page.getByTestId("custom-signup-form").getByTestId("message"),
    ).toContainText(`Signup successful. You can now login.`);
  });

  test("can log in after signed up with custom action", async ({ page }) => {
    await performLogin(page, {
      email,
      password,
    });

    await page.goto("/profile");
    await expect(
      page.getByTestId("user-profile").getByTestId("user-id"),
    ).toHaveText(email);
  });
});
