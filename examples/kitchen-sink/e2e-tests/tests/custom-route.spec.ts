import { expect, test } from "@playwright/test";
import { WASP_API_URL } from "../playwright.config";

test.describe("custom route", () => {
  // In dev mode this goes through the app URL, so it also covers the `server.proxy`
  // entry for `/customRoute` in the app's vite.config.ts.
  test("custom route from the server setup is reachable", async ({
    request,
  }) => {
    const response = await request.get(`${WASP_API_URL}/customRoute`);

    expect(response.ok()).toBe(true);
    expect(await response.text()).toBe("I am a custom route");
  });
});
