/**
 * This file describes additional tests for the `basic` starter.
 */

import { expect, test } from "@playwright/test";

test.describe(
  "basic only",
  {
    tag: "@basic",
  },
  () => {
    test("health check", async ({ page }) => {
      await page.goto("/");
      await expect(page.locator("body")).toBeVisible();
    });
  },
);
