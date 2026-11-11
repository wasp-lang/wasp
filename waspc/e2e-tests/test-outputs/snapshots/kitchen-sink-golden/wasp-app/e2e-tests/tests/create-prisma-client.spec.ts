import { expect, test } from "@playwright/test";
import { performLogin, setupTestUser } from "./auth";

test.describe("createPrismaClient", () => {
  const credentials = setupTestUser();

  test("custom Prisma client hides a specific task", async ({ page }) => {
    await performLogin(page, credentials);
    await expect(page).toHaveURL("/");

    await page.goto("/tasks");

    // Create a new task that will be hidden by the custom Prisma client
    const specificTask = "hidden by createPrismaClient";
    await page.locator("input[type='text']").fill(specificTask);
    await page.getByText("Create task").click();

    // Check that the save is submitted
    await expect(page.locator("input[type='text']")).toHaveValue("");

    await page.waitForLoadState("networkidle");

    await expect(page.locator("body")).not.toHaveText(specificTask);
  });
});
