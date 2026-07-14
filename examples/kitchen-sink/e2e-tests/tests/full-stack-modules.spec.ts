import { expect, test } from "@playwright/test";
import { performLogin, setupTestUser } from "./auth";

test.describe("full-stack modules", () => {
  const credentials = setupTestUser();

  test("submits the module job from a host-owned page", async ({ page }) => {
    await page.goto("/fsm-api");
    await page.getByRole("button", { name: "Submit module job" }).click();

    await expect(page.getByTestId("host-module-job-id")).toContainText(
      "Submitted job:",
    );
  });

  test("submits the module job from the module-owned page", async ({
    page,
  }) => {
    await performLogin(page, credentials);
    await page.goto("/fsm");
    await page.getByRole("button", { name: "Submit module job" }).click();

    await expect(page.getByTestId("module-job-id")).toContainText(
      "Submitted job:",
    );
  });
});
