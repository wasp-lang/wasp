import { expect, test } from "@playwright/test";
import {
  generateRandomCredentials,
  performLogin,
  performSignup,
} from "./helpers";

test.describe("auth and work tasks", () => {
  const { username, password } = generateRandomCredentials();

  test.describe.configure({ mode: "serial" });

  test("can sign up", async ({ page }) => {
    await performSignup(page, {
      username,
      password,
    });

    await expect(page).toHaveURL("/");

    await page.getByText("Logout").click();

    await expect(page).toHaveURL("/login");
  });

  test("can log in and interact with tasks", async ({ page }) => {
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

    const randomTask = `New Task ${Math.random().toString(36).substring(7)}`;
    await page.locator("input[name='description']").fill(randomTask);
    await page.locator("input[type='submit']").click();
    await expect(page.locator("body")).toContainText(randomTask);
    await page.locator("input[type='checkbox']").click();
    await page.reload();
    await expect(page.locator("input[type='checkbox']")).toBeChecked();
  });
});
