import { test, expect } from "@playwright/test";

test("has title", async ({ page }) => {
  await page.goto("http:/localhost:3000");

  await expect(page).toHaveTitle(/ToDo App/);
});

test("can sign up", async ({ page }) => {
  await page.goto("http://localhost:3000/signup");

  await page.waitForSelector("text=Create a new account");

  await page.locator("input[type='email']").fill("mihovil@ilakovac.com");
  await page.locator("input[type='password']").fill("12345678");
  await page.getByText("Sign up").click();

  await expect(page.locator("body")).toContainText(
    `You've signed up successfully! Check your email for the confirmation link.`
  );
});

test("can log in and create a task", async ({ page }) => {
  await page.goto("http://localhost:3000/login");

  await page.waitForSelector("text=Log in to your account");

  await page.locator("input[type='email']").fill("mihovil@ilakovac.com");
  await page.locator("input[type='password']").fill("12345678xxx");
  await page.getByRole("button", { name: "Log in" }).click();

  await expect(page.locator("body")).toContainText(`Invalid credentials`);

  await page.locator("input[type='password']").fill("12345678");
  await page.getByRole("button", { name: "Log in" }).click();

  await expect(page).toHaveURL("http://localhost:3000/profile");

  await page.goto("http://localhost:3000/");

  const randomTask = "New Task " + Math.random().toString(36).substring(7);
  await page.locator("input[type='text']").fill(randomTask);
  await page.getByText("Create new task").click();

  await expect(page.locator("body")).toContainText(randomTask);
});

// test("add new task", async ({ page }) => {
//   await page.goto("http://localhost:3000");

//   // Click the get started link.
//   await page.getByPlaceholder("Enter task").fill("New task");
//   await page.getByText("Create new task").click();

//   await page.waitForSelector("a[text=New task]");
//   await expect(page).toContain(/New task/);
// });
