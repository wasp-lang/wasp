import { expect, test } from "@playwright/test";

test.describe("operation errors", () => {
  test("the client receives the HttpError a query threw", async ({ page }) => {
    await page.goto("/errors");

    await expect(page.getByTestId("status")).toHaveText("418");
    await expect(page.getByTestId("message")).toHaveText("I'm a teapot");
    await expect(page.getByTestId("reason")).toHaveText("short and stout");
  });
});
