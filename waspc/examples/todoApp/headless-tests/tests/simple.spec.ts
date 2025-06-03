import { expect, test } from "@playwright/test";
import {
  generateRandomCredentials,
  performEmailVerification,
  performLogin,
  performSignup,
} from "./helpers";

test("has title", async ({ page }) => {
  await page.goto("/");

  await expect(page).toHaveTitle(/ToDo App/);
});
test.describe("signup and login", () => {
  const { email, password } = generateRandomCredentials();

  test.describe.configure({ mode: "serial" });

  test("social button renders", async ({ page }) => {
    await page.goto("/signup");

    await page.waitForSelector("text=Create a new account");

    await expect(
      page.locator("a[href='http://localhost:3001/auth/google/login']"),
    ).toBeVisible();
  });

  test("can sign up", async ({ page }) => {
    await performSignup(page, {
      email,
      password,
    });

    await expect(page.locator("body")).toContainText(
      `You've signed up successfully! Check your email for the confirmation link.`,
    );

    await performEmailVerification(page, email);
  });

  test("can log in and create a task", async ({ page }) => {
    await performLogin(page, {
      email,
      password: "12345678xxx",
    });

    await expect(page.locator("body")).toContainText("Invalid credentials");

    await performLogin(page, {
      email,
      password,
    });

    await expect(page).toHaveURL("/");

    await page.goto("/tasks");

    // Create a new task
    const randomTask = `New Task ${Math.random().toString(36).substring(7)}`;
    await page.locator("input[type='text']").fill(randomTask);
    await page.getByText("Create task").click();

    await expect(
      page.getByTestId("task-view").getByTestId("text"),
    ).toContainText(randomTask);
    await expect(
      page.getByTestId("task-view").getByTestId("created-by"),
    ).toContainText(`Created by ${email}`);

    // Navigate to task page
    page.getByTestId("task-view").getByTestId("text").click();
    await expect(
      page.getByTestId("task-detail").getByTestId("text"),
    ).toContainText(randomTask);
    await expect(
      page.getByTestId("task-detail").getByTestId("status"),
    ).toContainText("Pending");

    // Checking if Prisma enums work on the client
    await expect(
      page.getByTestId("task-detail").getByTestId("visibility"),
    ).toContainText("Visible to only you");
  });
});
