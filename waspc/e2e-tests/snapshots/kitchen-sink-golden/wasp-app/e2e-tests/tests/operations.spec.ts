import { expect, test } from "@playwright/test";
import { performLogin, setupTestUser } from "./auth";

test.describe("operations", () => {
  const credentials = setupTestUser();

  test("can create a task and visit details", async ({ page }) => {
    await performLogin(page, credentials);
    await expect(page).toHaveURL("/");

    await page.goto("/tasks");

    // Creating a new task
    const randomTask = `New Task ${Math.random().toString(36).substring(7)}`;
    await page.locator("input[type='text']").fill(randomTask);
    await page.getByText("Create task").click();

    await expect(
      page.getByTestId("task-view").getByTestId("text"),
    ).toContainText(randomTask);
    await expect(
      page.getByTestId("task-view").getByTestId("created-by"),
    ).toContainText(`Created by ${credentials.email}`);

    // Task details
    page.getByTestId("task-view").getByTestId("text").click();
    await expect(
      page.getByTestId("task-detail-view").getByTestId("description"),
    ).toContainText(randomTask);
    await expect(
      page.getByTestId("task-detail-view").getByTestId("status"),
    ).toContainText("Pending");

    // Checking if Prisma enums work on the client
    await expect(
      page.getByTestId("task-detail-view").getByTestId("visibility"),
    ).toContainText("Visible to only you");
  });
});
