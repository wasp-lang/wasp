import { test, expect } from "@playwright/test";
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

    await page.locator(".top-navbar button").click();

    await expect(page).toHaveURL("/login");
  });

  test("can log in and create a thought", async ({ page }) => {
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
    const randomThought = `New Thought ${Math.random()
      .toString(36)
      .substring(7)}`;
    const tag = "headless";
    await page.locator(".thought-tags-new input").fill(tag);
    await page.keyboard.press("Enter");
    await page.locator("textarea").fill(randomThought);
    await page.getByText("submit").click();

    const fullTaskText = `${randomThought}`;
    await page.waitForSelector(`.thought-list-view`);

    await expect(page.locator("body")).toContainText(fullTaskText);
    await expect(page.locator("body")).toContainText(`#${tag}`);
  });
});
