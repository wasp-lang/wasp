import { test, expect } from "@playwright/test";
import {
  generateRandomCredentials,
  performLogin,
  performSignup,
} from "./helpers";

test.describe("auth and work with tasks", () => {
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

  test("can log in and cast a vote", async ({ page }) => {
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
    // Fill input[name="description"] with random task
    await page.locator("input[name='description']").fill(randomTask);
    // Click input[type="submit"] to submit the form
    await page.locator("input[type='submit']").click();
    // Expect to see the task on the page
    await expect(page.locator("body")).toContainText(randomTask);
    // Check the task as done input[type="checkbox"]
    await page.locator("input[type='checkbox']").click();
    // Reload the page
    await page.reload();
    // Expect the task to be checked
    await expect(page.locator("input[type='checkbox']")).toBeChecked();
  });
});
