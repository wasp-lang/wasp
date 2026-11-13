import { expect, test } from "@playwright/test";
import { createVerifiedUser, gotoReady, logIn } from "./fixtures/users";

test("links CRUD in the dashboard", async ({ page }) => {
  const user = await createVerifiedUser();
  await logIn(page, user);
  await expect(page).toHaveURL(/\/links$/);
  await expect(page.getByText("No links yet")).toBeVisible();

  // Create (with a prefilled form, as an agent deep link would)
  await gotoReady(
    page,
    "/links/new?destinationUrl=https%3A%2F%2Fexample.com%2Fsale&title=Summer&tags=promo,q3",
  );
  await expect(page.getByLabel("Destination URL")).toHaveValue(
    "https://example.com/sale",
  );
  await page.getByLabel("Slug").fill("summer-sale");
  await page.getByRole("button", { name: "Create link" }).click();
  await expect(
    page.getByRole("heading", { name: "/r/summer-sale" }),
  ).toBeVisible();

  // Validation error shown inline
  await page.getByLabel("Destination URL").fill("not a url");
  await page.getByRole("button", { name: "Save changes" }).click();
  await expect(
    page.getByText("Must be an http:// or https:// URL."),
  ).toBeVisible();

  // Update
  await page.getByLabel("Destination URL").fill("https://example.com/new");
  await page.getByLabel("Title").fill("Summer 2026");
  await page.getByRole("button", { name: "Save changes" }).click();
  await expect(page.getByText("Saved")).toBeVisible();

  // List, search, tag filter
  await gotoReady(page, "/links");
  await expect(
    page.getByRole("link", { name: "/r/summer-sale" }),
  ).toBeVisible();
  await gotoReady(page, "/links?search=nomatch");
  await expect(page.getByText("No links match.")).toBeVisible();
  await gotoReady(page, "/links?tag=promo");
  await expect(
    page.getByRole("link", { name: "/r/summer-sale" }),
  ).toBeVisible();

  // Slug conflict error from the server
  await gotoReady(page, "/links/new");
  await page.getByLabel("Destination URL").fill("https://example.com/other");
  await page.getByLabel("Slug").fill("summer-sale");
  await page.getByRole("button", { name: "Create link" }).click();
  await expect(page.getByText(/already taken/)).toBeVisible();

  // Delete with the in-UI confirmation dialog
  await gotoReady(page, "/links");
  await page
    .getByRole("button", { name: "Actions for /r/summer-sale" })
    .click();
  await page.getByRole("menuitem", { name: "Delete" }).click();
  await page.getByRole("button", { name: "Delete link" }).click();
  await expect(page.getByText("No links yet")).toBeVisible();
});

test("another user's link is not found", async ({ page }) => {
  const owner = await createVerifiedUser();
  await logIn(page, owner);
  await gotoReady(page, "/links/new");
  await page.getByLabel("Destination URL").fill("https://example.com/private");
  await page.getByRole("button", { name: "Create link" }).click();
  await expect(page).toHaveURL(/\/links\/[0-9a-f-]{36}$/);
  const url = page.url();

  await page.getByRole("button", { name: "Log out" }).click();
  const other = await createVerifiedUser();
  await logIn(page, other);
  await gotoReady(page, url);
  await expect(page.getByText(/not found/)).toBeVisible();
});
