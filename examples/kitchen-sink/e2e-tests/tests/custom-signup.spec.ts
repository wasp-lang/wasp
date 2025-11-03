import { expect, test } from "@playwright/test";
import { performLogin, performSignup } from "./auth";
import { generateRandomEmail } from "./helpers";

test.describe("custom signup", () => {
  // We need the login test to run after the signup test.
  test.describe.configure({ mode: "serial" });

  const email = generateRandomEmail();
  const password = "12345678";

  test("can sign up ", async ({ page }) => {
    await performSignup(
      page,
      {
        email,
        password,
        address: "Some at least 10 letter address",
      },
      "custom-signup",
    );

    await expect(page.locator("body")).toContainText(
      `Signup successful. You can now login.`,
    );
  });

  test("can log in without email verification after custom sign up", async ({
    page,
  }) => {
    await performLogin(page, {
      email,
      password,
    });
    await expect(page).toHaveURL("/");

    await page.goto("/profile");
    await expect(
      page.getByTestId("user-profile").getByTestId("user-id"),
    ).toHaveText(email);
  });
});
