/**
 * This file describes additional tests for the `basic` starter.
 */

import { expect, type Locator, type Page, test } from "@playwright/test";
import { randomUUID } from "crypto";
import { isRunningInDevMode, performLogin, setupTestUser } from "../auth.js";

test.describe(
  "basic only",
  {
    tag: "@basic",
  },
  () => {
    function getTaskForm(page: Page): Locator {
      return page.locator("#create-task");
    }

    function getTasksSection(page: Page): Locator {
      return page
        .locator(
          "section.card.flex.w-full.max-w-3xl.flex-col.gap-6.p-4.lg\\:p-6",
        )
        .nth(1);
    }

    function getTagDialog(page: Page): Locator {
      return page.getByRole("dialog");
    }

    test.describe("welcome page", () => {
      test("should show the walkthrough", async ({ page }) => {
        await page.goto("/");

        await expect(
          page.getByText("Your app is up and running"),
        ).toBeVisible();
        await expect(page.getByText("Not logged in")).toBeVisible();
      });

      test("should create a sample user and log in with it", async ({
        page,
      }) => {
        test.skip(
          !isRunningInDevMode(),
          "The sample user is only available in development.",
        );
        await page.goto("/");

        await page
          .getByRole("button", { name: "Create a sample user" })
          .click();
        await expect(page.getByText("sample@example.com")).toBeVisible();

        await performLogin(page, {
          email: "sample@example.com",
          password: "password123",
        });
        await expect(page.getByText("Logged in as sampleuser")).toBeVisible();
      });
    });

    test.describe("tasks", () => {
      test.describe.configure({ mode: "serial" });

      const credentials = setupTestUser({
        username: "Test User",
      });
      const tagName = `Test tag: ${randomUUID()}`;

      test.beforeEach(async ({ page }) => {
        await performLogin(page, credentials);
        await page.goto("/tasks");
      });

      test("should have state for no tasks", async ({ page }) => {
        const tasksSection = getTasksSection(page);

        await expect(tasksSection.getByText("No tasks found.")).toBeVisible();
      });

      test("should be able to create a task without tags", async ({ page }) => {
        const taskWithoutTagDescription = `Test task without tags: ${randomUUID()}`;
        const taskForm = getTaskForm(page);
        const tasksSection = getTasksSection(page);

        await taskForm
          .getByLabel("Description")
          .fill(taskWithoutTagDescription);
        await taskForm.getByRole("button", { name: "Create" }).click();
        await page.waitForLoadState("networkidle");

        await expect(
          tasksSection.getByText(taskWithoutTagDescription),
        ).toBeVisible();
        await expect(tasksSection.getByText("1 task")).toBeVisible();
        await expect(tasksSection.getByText("0 completed")).toBeVisible();
      });

      test("should be able to create a tag", async ({ page }) => {
        const taskForm = getTaskForm(page);
        const tagForm = getTagDialog(page);

        await taskForm.getByRole("button", { name: "Add a Tag" }).click();
        await tagForm.getByLabel("Name").fill(tagName);
        await tagForm.getByRole("button", { name: "Create" }).click();
        await page.waitForLoadState("networkidle");

        await expect(
          taskForm.getByRole("button", { name: tagName }),
        ).toBeVisible();
      });

      test("should be able to create task with a tag", async ({ page }) => {
        const taskWithTagDescription = `Test task with tag: ${randomUUID()}`;
        const taskForm = getTaskForm(page);
        const tasksSection = getTasksSection(page);

        await taskForm.getByLabel("Description").fill(taskWithTagDescription);
        await taskForm.getByRole("button", { name: tagName }).click();
        await taskForm.getByRole("button", { name: "Create" }).click();
        await page.waitForLoadState("networkidle");

        await expect(
          tasksSection.getByText(taskWithTagDescription),
        ).toBeVisible();
        await expect(tasksSection.getByText("2 tasks")).toBeVisible();
        await expect(tasksSection.getByText("0 completed")).toBeVisible();
      });

      test("should be able to check tasks", async ({ page }) => {
        const tasksSection = getTasksSection(page);
        // Wait for the list to load, since `.all()` below doesn't auto-wait.
        await expect(tasksSection.getByText("2 tasks")).toBeVisible();

        const taskCheckboxes = await tasksSection.getByRole("checkbox").all();
        for (const checkbox of taskCheckboxes) {
          // We don't want to use `checkbox.check()` here because playwright
          // fails the test if the checkbox is not checked right after.
          // We fail the test because we use controlled components, and
          // click on the checkbox starts an async request to the server instead.
          // Once the cache is revalidated, the checkbox will be checked.
          await checkbox.click();
        }
        await page.waitForLoadState("networkidle");

        await expect(tasksSection.getByText("2 tasks")).toBeVisible();
        await expect(tasksSection.getByText("2 completed")).toBeVisible();
      });

      test("should be able to clear completed tasks", async ({ page }) => {
        const tasksSection = getTasksSection(page);

        await tasksSection
          .getByRole("button", { name: "Clear completed" })
          .click();
        await page.waitForLoadState("networkidle");

        await expect(tasksSection.getByText("No tasks found.")).toBeVisible();
      });
    });
  },
);
