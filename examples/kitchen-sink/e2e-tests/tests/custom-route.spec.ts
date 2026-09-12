import { expect, test } from "@playwright/test";
import { WASP_APP_URL } from "../playwright.config";

test.describe("custom route", () => {
  test("custom route from the server setup is reachable", async ({
    request,
  }) => {
    const response = await request.get(`${WASP_APP_URL}/customRoute`);

    expect(response.ok()).toBe(true);
    expect(await response.text()).toBe("I am a custom route");
  });
});
