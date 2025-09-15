import { expect, test } from "@playwright/test";
import { performLogin, setupTestUser } from "./auth";

test.describe("async jobs", () => {
  const credentials = setupTestUser();

  test("submits a job and gets result", async ({ page }) => {
    await performLogin(page, credentials);
    await expect(page).toHaveURL("/");

    await page.goto("/jobs");

    await expect(page).toHaveURL("/jobs");

    const text = `Some text ${Math.random().toString(36).substring(7)}`;
    await page.getByTestId("jobs-payload-input").fill(text);
    await page.getByRole("button", { name: "Submit Job" }).click();

    await expect(
      page.getByTestId("job-request").getByTestId("input"),
    ).toHaveText(`"${text}"`);
    await expect(
      page.getByTestId("job-request").getByTestId("status"),
    ).toHaveText("pending");

    await expect(
      page.getByTestId("job-request").getByTestId("status"),
    ).toHaveText("success");
    await expect(
      page.getByTestId("job-request").getByTestId("output"),
    ).toHaveText(`"${text.toUpperCase()}"`);
  });
});
