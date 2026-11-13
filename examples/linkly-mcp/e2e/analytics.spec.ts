import { expect, test } from "@playwright/test";
import { SERVER_URL } from "./env";
import { createVerifiedUser, gotoReady, logIn } from "./fixtures/users";

test("short URLs redirect, record clicks, and show in zero-filled stats", async ({
  page,
}) => {
  const user = await createVerifiedUser();
  await logIn(page, user);

  const slug = `go-${Date.now()}`;
  await gotoReady(page, "/links/new");
  await page.getByLabel("Destination URL").fill("https://example.com/landing");
  await page.getByLabel("Slug").fill(slug);
  await page.getByRole("button", { name: "Create link" }).click();
  await expect(page.getByRole("heading", { name: `/r/${slug}` })).toBeVisible();
  const linkPath = new URL(page.url()).pathname;

  // Visiting the short URL redirects (302) and records a click.
  for (let i = 0; i < 2; i++) {
    const res = await fetch(`${SERVER_URL}/r/${slug}`, {
      redirect: "manual",
      headers: { Referer: "https://news.example.org/post" },
    });
    expect(res.status).toBe(302);
    expect(res.headers.get("location")).toBe("https://example.com/landing");
  }
  const missing = await fetch(`${SERVER_URL}/r/does-not-exist-${Date.now()}`, {
    redirect: "manual",
  });
  expect(missing.status).toBe(404);

  // Range and groupBy come from URL params; days are zero-filled.
  await expect
    .poll(async () => {
      await gotoReady(page, `${linkPath}?tab=stats&range=30d&groupBy=day`);
      return page.getByTestId("total-clicks").textContent();
    })
    .toBe("2");
  await expect(page.getByRole("combobox", { name: "Time range" })).toHaveText(
    "Last 30 days",
  );
  const rows = page
    .getByRole("table", { name: "Clicks data" })
    .locator("tbody tr");
  await expect(rows).toHaveCount(30);
  const zeroDays = await rows.evaluateAll(
    (trs) =>
      trs.filter((tr) => tr.lastElementChild?.textContent === "0").length,
  );
  expect(zeroDays).toBe(29);

  await gotoReady(page, `${linkPath}?tab=stats&range=7d&groupBy=referrer`);
  await expect(page.getByRole("tab", { name: "By referrer" })).toHaveAttribute(
    "aria-selected",
    "true",
  );
  await expect(
    page.getByRole("table", { name: "Clicks data" }).locator("tbody tr"),
  ).toHaveText([/news\.example\.org\s*2/]);

  // Top links
  await gotoReady(page, "/analytics?range=24h");
  await expect(page.getByRole("link", { name: `/r/${slug}` })).toBeVisible();
});
