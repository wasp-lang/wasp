import { expect, test } from "@playwright/test";
import {
  generateRandomCredentials,
  performLogin,
  performSignup,
} from "./helpers";

test.describe("auth and simple usage", () => {
  const { username, password } = generateRandomCredentials();

  test.describe.configure({ mode: "serial" });

  test("can sign up", async ({ page }) => {
    await performSignup(page, {
      username,
      password,
    });

    await expect(page).toHaveURL("/");

    await page.locator(".logout-btn").click();

    await expect(page).toHaveURL("/login");
  });

  test("can log in and create lists and cards", async ({ page }) => {
    await performLogin(page, {
      username,
      password: "12345678xxx",
    });

    await expect(page.locator("body")).toContainText("Invalid credentials");

    await performLogin(page, {
      username,
      password,
    });

    await expect(page).toHaveURL("/");

    // Create first list
    await page.getByRole("button", { name: "Add a list" }).click();
    await page
      .getByRole("textbox", { name: "Enter list title..." })
      .fill("New list");
    await page.getByRole("button", { name: "Add list" }).click();
    await expect(page.locator(".list-header-name").nth(0)).toContainText(
      "New list",
    );

    // Create first card
    await page.getByRole("button", { name: "Add a card", exact: true }).click();
    await page
      .getByRole("textbox", { name: "Enter a title for this card..." })
      .fill("New card");
    await page.getByRole("button", { name: "Add card", exact: true }).click();
    await expect(page.locator(".list-card-title").nth(0)).toContainText(
      "New card",
    );

    // Create second card
    await page
      .getByRole("textbox", { name: "Enter a title for this card..." })
      .click();
    await page
      .getByRole("textbox", { name: "Enter a title for this card..." })
      .fill("Extra card");
    await page.getByRole("button", { name: "Add card", exact: true }).click();
    await expect(page.locator(".list-card-title").nth(1)).toContainText(
      "Extra card",
    );

    // Create second list
    await page.getByRole("textbox", { name: "Enter list title..." }).click();
    await page
      .getByRole("textbox", { name: "Enter list title..." })
      .fill("Extra list");
    await page.getByRole("button", { name: "Add list" }).click();
    await expect(page.locator(".list-header-name").nth(1)).toContainText(
      "Extra list",
    );
  });
});
