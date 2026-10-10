import { expect, test } from "@playwright/test";
import { WASP_SERVER_URL } from "../playwright.config";

test.describe("server errors", () => {
  test("an unexpected error becomes a generic 500", async ({ request }) => {
    const response = await request.get(`${WASP_SERVER_URL}/errors/unexpected`);

    expect(response.status()).toBe(500);
    expect(await response.json()).toEqual({
      message: "Internal server error",
    });
  });

  test("an HttpError keeps its status, message, and data", async ({
    request,
  }) => {
    const response = await request.get(`${WASP_SERVER_URL}/errors/http`);

    expect(response.status()).toBe(418);
    expect(await response.json()).toEqual({
      message: "I'm a teapot",
      data: { reason: "short and stout" },
    });
  });

  test("a malformed JSON body keeps its 400 status", async ({ request }) => {
    const response = await request.patch(`${WASP_SERVER_URL}/bar/baz`, {
      headers: { "Content-Type": "application/json" },
      data: "{ not json",
    });

    expect(response.status()).toBe(400);
    expect(await response.json()).toEqual({
      message: expect.any(String),
    });
  });
});
