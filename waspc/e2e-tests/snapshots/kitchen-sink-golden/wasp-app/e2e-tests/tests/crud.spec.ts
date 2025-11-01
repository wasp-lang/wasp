import { expect, type Page, test } from "@playwright/test";
import { performLogin, setupTestUser } from "./auth";

test.describe("CRUD test", () => {
  const credentials = setupTestUser();

  test("crud list page", async ({ page }) => {
    await performLogin(page, credentials);
    await expect(page).toHaveURL("/");

    await page.goto("/crud");
    await expect(page.getByTestId("crud-tasks")).toBeVisible();

    // Create a task
    const taskDescription = "first task";
    await createTask(page, taskDescription);

    await expect(
      page.getByTestId("task-view").getByTestId("text"),
    ).toContainText(taskDescription);
    await expect(
      page.getByTestId("task-view").getByTestId("created-by"),
    ).toContainText(`Created by ${credentials.email}`);

    // Edit the task
    await page.getByRole("button", { name: "Edit" }).click();
    const editInput = page.getByTestId("edit-task-input");
    const newTaskDescription = "edited task";

    await expect(editInput).toHaveValue(taskDescription);
    await editInput.fill(newTaskDescription);
    await page.getByRole("button", { name: "Update task" }).click();
    await expect(
      page.getByTestId("task-view").getByTestId("text"),
    ).toContainText(newTaskDescription);
    await expect(
      page.getByTestId("task-view").getByTestId("created-by"),
    ).toContainText(`Created by ${credentials.email}`);

    // Delete the task
    page.on("dialog", async (dialog) => {
      expect(dialog.message()).toBe(
        "Are you sure you want to delete this task?",
      );
      await dialog.accept();
    });
    await page.locator("button").filter({ hasText: "Delete" }).click();
    await page.waitForLoadState("networkidle");

    await expect(
      page.getByTestId("task-view").getByTestId("text"),
    ).not.toBeVisible();
    await expect(
      page.getByTestId("task-view").getByTestId("created-by"),
    ).not.toBeVisible();
    await expect(page.getByTestId("no-tasks-message")).toBeVisible();
  });

  test("crud detail page", async ({ page }) => {
    await performLogin(page, credentials);
    await expect(page).toHaveURL("/");

    await page.goto("/crud");
    // Create a task
    await createTask(page, "second task");
    // Go to the detail page of the task
    await page.locator("a").filter({ hasText: "second task" }).click();
    await expect(page).toHaveURL(/\/crud\/\d+/);
    // Check if the task is displayed
    // await expect(page.locator("body")).toContainText("second task");
    await expect(page.getByTestId("task-detail-view")).toContainText(
      "second task",
    );
  });
});

async function createTask(page: Page, description: string) {
  await page.getByRole("textbox").fill(description);
  await page.getByRole("button", { name: "Create task" }).click();
}
