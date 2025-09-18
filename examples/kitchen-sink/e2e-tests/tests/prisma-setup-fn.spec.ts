import { expect, test } from "@playwright/test";
import { performLogin, setupTestUser } from "./auth";

test.describe("prisma setup fn", () => {
  const credentials = setupTestUser();

  test("prisma setup hook hides a specific task", async ({ page }) => {
    await performLogin(page, credentials);
    await expect(page).toHaveURL("/");

    await page.goto("/tasks");

    // Create a new task that will be hidden by the Prisma setup function
    const specificTask = "hidden by setUpPrisma";
    await page.locator("input[type='text']").fill(specificTask);
    await page.getByText("Create task").click();

    // Check that the save is submitted
    await expect(page.locator("input[type='text']")).toHaveValue("");

    await page.waitForLoadState("networkidle");

    await expect(page.locator("body")).not.toHaveText(specificTask);
  });
});
