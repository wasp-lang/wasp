import { expect, test } from "@playwright/test";
import {
  generateRandomCredentials,
  performEmailVerification,
  performLogin,
  performSignup,
} from "./helpers";

test.describe("websocket", () => {
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

  test("chat works", async ({ page }) => {
    await performLogin(page, {
      email,
      password,
    });

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
    await expect(message).toContainText(email);
  });
});
