import { expect, type Page, test } from "@playwright/test";

test.describe("client errors", () => {
  test("reports an error thrown in an event handler", async ({ page }) => {
    const reports = collectClientErrorReports(page);
    await page.goto("/errors/client");

    await page
      .getByRole("button", { name: "Throw in an event handler" })
      .click();

    await expectSingleReport(reports, {
      source: "window",
      message: "Thrown in an event handler",
    });
  });

  test("reports a rejected promise nobody caught", async ({ page }) => {
    const reports = collectClientErrorReports(page);
    await page.goto("/errors/client");

    await page.getByRole("button", { name: "Reject a promise" }).click();

    await expectSingleReport(reports, {
      source: "window",
      message: "Rejected without a catch",
    });
  });

  test("reports an error thrown while rendering once", async ({ page }) => {
    const reports = collectClientErrorReports(page);
    await page.goto("/errors/client");

    await page.getByRole("button", { name: "Throw while rendering" }).click();

    await expect(
      page.getByText("There was an error rendering this page"),
    ).toBeVisible();
    await expectSingleReport(reports, {
      source: "render",
      message: "Thrown while rendering",
    });
  });
});

function collectClientErrorReports(page: Page): string[] {
  const reports: string[] = [];
  page.on("console", (message) => {
    if (
      message.type() === "error" &&
      message.text().startsWith("Unexpected error in")
    ) {
      reports.push(message.text());
    }
  });
  return reports;
}

async function expectSingleReport(
  reports: string[],
  { source, message }: { source: string; message: string },
): Promise<void> {
  await expect.poll(() => reports.length).toBe(1);
  expect(reports[0]).toContain(`Unexpected error in ${source}:`);
  expect(reports[0]).toContain(message);
}
