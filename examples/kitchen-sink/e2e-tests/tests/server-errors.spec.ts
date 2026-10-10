import { expect, test } from "@playwright/test";
import { WASP_SERVER_URL } from "../playwright.config";

test.describe("server errors", () => {
  test("an unexpected error becomes a generic 500", async ({ request }) => {
    const response = await request.get(`${WASP_SERVER_URL}/errors/unexpected`);

    expect(response.status()).toBe(500);
    expect(await response.json()).toEqual({
      message: "Internal Server Error",
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

  test("a middleware error keeps its status but hides a private message", async ({
    request,
  }) => {
    const response = await request.get(`${WASP_SERVER_URL}/errors/concealed`);

    expect(response.status()).toBe(401);
    expect(await response.json()).toEqual({ message: "Unauthorized" });
  });

  test("a middleware 5xx error keeps its status", async ({ request }) => {
    const response = await request.get(`${WASP_SERVER_URL}/errors/unavailable`);

    expect(response.status()).toBe(503);
    expect(await response.json()).toEqual({ message: "Service Unavailable" });
  });

  test("a middleware error keeps its headers", async ({ request }) => {
    const response = await request.get(`${WASP_SERVER_URL}/errors/rate-limit`);

    expect(response.status()).toBe(429);
    expect(response.headers()["retry-after"]).toBe("120");
    expect(await response.json()).toEqual({
      message: "Too many requests, slow down",
    });
  });
});
