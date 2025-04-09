import { test, expect } from "@playwright/test";
import {
  generateRandomCredentials,
  performLogin,
  performSignup,
} from "./helpers";

test("has title", async ({ page }) => {
  await page.goto("/");

  await expect(page).toHaveTitle(/ToDo TypeScript/);
});
test.describe("signup and login", () => {
  const { username, password } = generateRandomCredentials();

  test.describe.configure({ mode: "serial" });

  test("can sign up", async ({ page }) => {
    await performSignup(page, {
      username,
      password,
    });

    await expect(page).toHaveURL("/");

    await page.locator("button.logout").click();

    await expect(page).toHaveURL("/login");
  });

  test("can log in and create a task", async ({ page }) => {
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

    // Create a new task
    const randomTask = `New Task ${Math.random().toString(36).substring(7)}`;
    await page.locator("input[type='text']").fill(randomTask);
    await page.getByText("Create task").click();

    const fullTaskText = `${randomTask}`;
    await page.waitForSelector(`text=${fullTaskText}`);

    await expect(page.locator("body")).toContainText(fullTaskText);
  });
});
