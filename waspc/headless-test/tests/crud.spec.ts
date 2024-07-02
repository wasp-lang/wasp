import { test, expect } from "@playwright/test";

test.describe("CRUD test", () => {
  const randomEmail = `test${Math.random().toString(36).substring(7)}@test.com`;
  const password = "12345678";

  test.describe.configure({ mode: "serial" });

  test.beforeAll(async ({ browser }) => {
    const page = await browser.newPage();

    // Sign up
    await page.goto("/signup");

    await page.waitForSelector("text=Create a new account");

    await page.locator("input[type='email']").fill(randomEmail);
    await page.locator("input[type='password']").fill(password);
    await page.locator("button").click();
  });

  test("CRUD with override works", async ({ page }) => {
    await page.goto("/login");

    await page.waitForSelector("text=Log in to your account");

    await page.locator("input[type='email']").fill(randomEmail);
    await page.locator("input[type='password']").fill(password);
    await page.getByRole("button", { name: "Log in" }).click();

    await page.waitForSelector("text=Profile page");

    await page.goto("/crud");

    await page.waitForSelector("text=Tasks");

    await createTask(page, "special filter 1");
    await createTask(page, "special filter 2");
    await createTask(page, "something else");

    await expect(page.locator("body")).toContainText("special filter 1");
    await expect(page.locator("body")).toContainText("special filter 2");
    await expect(page.locator("li[text='something else']")).not.toBeVisible();
  });
});

async function createTask(page: any, description: string) {
  await page.locator("input[type='text']").fill(description);
  await page.getByText("Create task").click();
}
