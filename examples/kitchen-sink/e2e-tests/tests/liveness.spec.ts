import { expect, test } from "@playwright/test";
import { WASP_SERVER_URL } from "../playwright.config";

test.describe("liveness check", () => {
  test("the server answers the liveness check", async ({ request }) => {
    const response = await request.get(`${WASP_SERVER_URL}/up`);

    expect(response.status()).toBe(200);
    expect(await response.json()).toEqual({ status: "ok" });
  });

  test("the server root is not a liveness check anymore", async ({
    request,
  }) => {
    const response = await request.get(`${WASP_SERVER_URL}/`);

    // In development the root shows the wrong-port page, in production it is a 404.
    // Either way it no longer answers with the empty 200 the old liveness check relied on.
    expect(await response.text()).not.toBe("");
  });
});
