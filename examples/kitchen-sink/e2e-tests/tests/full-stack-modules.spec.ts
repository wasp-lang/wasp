import { expect, test } from "@playwright/test";
import { performLogin, setupTestUser } from "./auth";

test.describe("full-stack modules", () => {
  const credentials = setupTestUser();

  test("pings the module API from a host-owned page", async ({ page }) => {
    await page.goto("/fsm-api");
    await page.getByRole("button", { name: "Ping module API" }).click();

    await expect(page.getByTestId("host-module-api-header")).toContainText(
      "@kitchen-sink/module",
    );
  });

  test("submits the module job from a host-owned page", async ({ page }) => {
    await page.goto("/fsm-api");
    await page.getByRole("button", { name: "Submit module job" }).click();

    await expect(page.getByTestId("host-module-job-id")).toContainText(
      "Submitted job:",
    );
  });

  test("pings the module API from the module-owned page", async ({ page }) => {
    await performLogin(page, credentials);
    await expect(page).toHaveURL("/");
    await page.goto("/fsm");
    await page.getByRole("button", { name: "Ping module API" }).click();

    await expect(page.getByTestId("module-api-header")).toContainText(
      "@kitchen-sink/module",
    );
  });

  test("submits the module job from the module-owned page", async ({
    page,
  }) => {
    await performLogin(page, credentials);
    await expect(page).toHaveURL("/");
    await page.goto("/fsm");
    await page.getByRole("button", { name: "Submit module job" }).click();

    await expect(page.getByTestId("module-job-id")).toContainText(
      "Submitted job:",
    );
  });

  test("manages TODOs through the module CRUD", async ({ page }) => {
    await performLogin(page, credentials);
    await expect(page).toHaveURL("/");
    await page.goto("/fsm");

    const crudItems = page.getByTestId("module-crud-item");
    await expect(page.getByTestId("module-crud-list")).toBeAttached();
    const initialCount = await crudItems.count();

    await page.getByRole("button", { name: "Add one random TODO" }).click();
    await expect(crudItems).toHaveCount(initialCount + 1);

    // The CRUD getAll override orders by id descending, so the new TODO is first.
    const newTodo = crudItems.first();
    await expect(newTodo).toHaveAttribute("data-done", "false");

    await newTodo.getByRole("button", { name: "Toggle done" }).click();
    await expect(newTodo).toHaveAttribute("data-done", "true");

    await newTodo.getByRole("button", { name: "Delete" }).click();
    await expect(crudItems).toHaveCount(initialCount);
  });
});
