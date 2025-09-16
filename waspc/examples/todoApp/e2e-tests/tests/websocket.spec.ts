import { expect, test } from "@playwright/test";
import { performLogin, setupTestUser } from "./auth";

test.describe("websocket", () => {
  const credentials = setupTestUser();

  test("chat works", async ({ page }) => {
    await performLogin(page, credentials);
    await expect(page).toHaveURL("/");

    await page.goto("/chat");

    await expect(page).toHaveURL("/chat");

    await page
      .getByRole("textbox", { name: "Type your message..." })
      .fill("Hello World!");

    await page.getByRole("button", { name: "Send" }).click();

    await expect(page.getByText("Hello World!")).toBeVisible();

    const message = page.getByTestId("message");
    await expect(message).toHaveCount(1);
    await expect(message).toContainText("Hello World!");
    await expect(message).toContainText(credentials.email);
  });
});
