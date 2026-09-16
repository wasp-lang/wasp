import { expect, test } from "@playwright/test";
import { WASP_SERVER_URL } from "../playwright.config";

test.describe("custom API HTTP methods", () => {
  test("a PATCH api answers PATCH requests", async ({ request }) => {
    const response = await request.patch(`${WASP_SERVER_URL}/bar/baz`, {
      data: { answer: 42 },
    });

    expect(response.ok()).toBe(true);
    expect(await response.json()).toEqual({
      msg: 'Patched with {"answer":42}',
    });
  });

  test("a HEAD api answers HEAD requests", async ({ request }) => {
    const response = await request.head(`${WASP_SERVER_URL}/bar/baz`);

    expect(response.ok()).toBe(true);
    expect(response.headers()["x-api-handler"]).toBe("headBarBaz");
  });

  test("an OPTIONS api answers OPTIONS requests", async ({ request }) => {
    const response = await request.fetch(`${WASP_SERVER_URL}/foo/baz`, {
      method: "OPTIONS",
    });

    expect(response.ok()).toBe(true);
    expect(response.headers()["allow"]).toBe("OPTIONS");
  });

  test("a GET api at the same path still answers GET requests", async ({
    request,
  }) => {
    const response = await request.get(`${WASP_SERVER_URL}/bar/baz`);

    expect(response.ok()).toBe(true);
    expect(await response.json()).toEqual({ msg: "Hello, stranger!" });
  });
});
