import { expect, test } from "@playwright/test";
import { performLogin, setupTestUser } from "./auth";

test.describe("user API", () => {
  const credentials = setupTestUser();

  test("user API works on the client", async ({ page }) => {
    await performLogin(page, credentials);
    await expect(page).toHaveURL("/");

    await page.goto("/profile");

    await expect(page).toHaveURL("/profile");

    await expect(
      page.getByTestId("user-profile").getByTestId("user-id"),
    ).toContainText(credentials.email);
    await expect(
      page.getByTestId("user-profile").getByTestId("email-status"),
    ).toContainText("Verified");
  });
});
