import { expect, test } from "@playwright/test";
import {
  generateRandomCredentials,
  performEmailVerification,
  performLogin,
  performSignup,
} from "./helpers";

test.describe("async jobs", () => {
  const { email, password } = generateRandomCredentials();

  test.describe.configure({ mode: "serial" });

  test.beforeAll(async ({ browser }) => {
    const page = await browser.newPage();

    await performSignup(page, {
      email,
      password,
    });

    await expect(page.locator("body")).toContainText(
      `You've signed up successfully! Check your email for the confirmation link.`,
    );

    await performEmailVerification(page, email);
  });

  test("submits a job and gets result", async ({ page }) => {
    await performLogin(page, {
      email,
      password,
    });

    await expect(page).toHaveURL("/");

    await page.goto("/jobs");

    await expect(page).toHaveURL("/jobs");

    const text = `Some text ${Math.random().toString(36).substring(7)}`;
    await page.getByTestId("jobs-payload-input").fill(text);
    await page.getByRole("button", { name: "Submit Job" }).click();

    await expect(
      page.getByTestId("job-request").getByTestId("input"),
    ).toContainText(text);
    await expect(
      page.getByTestId("job-request").getByTestId("status"),
    ).toContainText("pending");

    await page.waitForTimeout(2000);

    await expect(
      page.getByTestId("job-request").getByTestId("status"),
    ).toContainText("success");
    await expect(
      page.getByTestId("job-request").getByTestId("output"),
    ).toContainText(text.toUpperCase());
  });
});
