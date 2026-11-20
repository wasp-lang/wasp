/// <reference lib="dom" />

import { expect, test } from "@playwright/test";
import { readFile } from "node:fs/promises";

test("starter and tutorial base styles stay in sync", async () => {
  const readRepoFile = (path: string) =>
    readFile(new URL(`../../../${path}`, import.meta.url), "utf8");
  const minimal = await readRepoFile(
    "waspc/data/Cli/starters/minimal/src/Main.css",
  );
  expect(await readRepoFile("waspc/data/Cli/starters/basic/src/Main.css")).toBe(
    minimal,
  );
  for (const app of ["TodoApp", "TodoAppTs"]) {
    expect(
      (await readRepoFile(`examples/tutorials/${app}/src/Main.css`)).startsWith(
        minimal,
      ),
    ).toBe(true);
  }
});

for (const colorScheme of ["light", "dark"] as const) {
  test.describe(`${colorScheme} theme`, () => {
    test.use({ colorScheme });

    test("follows the system theme without overflowing on small screens", async ({
      page,
    }) => {
      await page.setViewportSize({ width: 320, height: 740 });
      await page.goto("/");

      await expect(page.locator("body")).toHaveCSS(
        "background-color",
        colorScheme === "dark" ? "rgb(24, 24, 24)" : "rgb(247, 245, 240)",
      );
      await expect(page.locator("body")).toHaveCSS(
        "color",
        colorScheme === "dark" ? "rgb(247, 245, 240)" : "rgb(17, 17, 17)",
      );
      expect(
        await page.evaluate(
          () => document.documentElement.scrollWidth <= window.innerWidth,
        ),
      ).toBe(true);
      await expect(page.locator("main .button").first()).toHaveCSS(
        "align-items",
        "center",
      );
    });

    test(
      "auth fields and password controls follow the app theme",
      { tag: "@basic" },
      async ({ page }) => {
        await page.goto("/login");
        const email = page.locator("input[name='email']");
        await expect(email).toHaveCSS(
          "background-color",
          colorScheme === "dark" ? "rgb(34, 34, 34)" : "rgb(250, 250, 250)",
        );
        await expect(email).toHaveCSS(
          "color",
          colorScheme === "dark" ? "rgb(247, 245, 240)" : "rgb(17, 17, 17)",
        );
        await email.focus();
        await expect(email).toHaveCSS("outline-style", "solid");
        const inputHeight = await email.evaluate(
          (input) => input.getBoundingClientRect().height,
        );
        const submitHeight = await page
          .getByRole("button", { name: "Log in", exact: true })
          .evaluate((button) => button.getBoundingClientRect().height);
        expect(submitHeight).toBe(inputHeight);
        await expect(
          page.getByRole("button", { name: "Show password" }),
        ).toHaveCSS(
          "color",
          colorScheme === "dark" ? "rgb(247, 245, 240)" : "rgb(17, 17, 17)",
        );
      },
    );
  });
}
