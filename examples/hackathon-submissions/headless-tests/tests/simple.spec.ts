import { test, expect } from "@playwright/test";

test("create a new hackathon submission", async ({ page }) => {
  await page.goto("/");
  await page
    .getByRole("textbox", { name: "Project Name *" })
    .fill("New project");
  await page
    .getByRole("textbox", { name: "Email address *" })
    .fill("test@test.com");
  await page
    .getByRole("textbox", { name: "Countries Represented" })
    .fill("USA");
  await page
    .getByRole("textbox", { name: "Countries Represented" })
    .press("Tab");
  await page
    .getByRole("textbox", { name: "GitHub Repo *" })
    .fill("github.com/something/something");
  await page.getByRole("textbox", { name: "Website" }).fill("something.com");
  await page
    .getByRole("textbox", { name: "Twitter" })
    .fill("twitter.com/something");
  await page
    .getByRole("textbox", { name: "Description *" })
    .fill("Some description");

  await page.getByRole("button", { name: "Save" }).click();
  await expect(page.locator(".project")).toContainText("New project");
  await expect(page.locator(".project")).toContainText("Some description");
});
