import { expect, test } from "@playwright/test";
import { WASP_SERVER_URL } from "../playwright.config";

test.describe("liveness check", () => {
  test("the server answers the liveness check", async ({ request }) => {
    const response = await request.get(`${WASP_SERVER_URL}/up`);

    expect(response.status()).toBe(200);
    expect(await response.json()).toEqual({ status: "ok" });
  });
});
