import { test, expect } from "@playwright/test";

test("has title", async ({ page }) => {
  await page.goto("http:/localhost:3000");

  await expect(page).toHaveTitle(/ToDo App/);
});
test.describe("signup and login", () => {
  const randomEmail =
    "test" + Math.random().toString(36).substring(7) + "@test.com";
  const password = "12345678";

  test.describe.configure({ mode: "serial" });

  test("social button renders", async ({ page }) => {
    await page.goto("/signup");

    await page.waitForSelector("text=Create a new account");

    await expect(page.locator("a[href='http://localhost:3001/auth/google/login']")).toBeVisible();
  });

  test("can sign up", async ({ page }) => {
    await page.goto("/signup");

    await page.waitForSelector("text=Create a new account");

    await page.locator("input[type='email']").fill(randomEmail);
    await page.locator("input[type='password']").fill(password);
    await page.locator("button").click();

    await expect(page).toHaveURL("/profile");
  });

  test("can log in and create a task", async ({ page }) => {
    await page.goto("/login");

    await page.waitForSelector("text=Log in to your account");

    await page.locator("input[type='email']").fill(randomEmail);
    await page.locator("input[type='password']").fill("12345678xxx");
    await page.getByRole("button", { name: "Log in" }).click();

    await expect(page.locator("body")).toContainText(`Invalid credentials`);

    await page.locator("input[type='password']").fill(password);
    await page.getByRole("button", { name: "Log in" }).click();

    await expect(page).toHaveURL("/profile");

    await page.goto("/");

    const randomTask = "New Task " + Math.random().toString(36).substring(7);
    await page.locator("input[type='text']").fill(randomTask);
    await page.getByText("Create new task").click();

    await expect(page.locator("body")).toContainText(randomTask);
  });
});
