import { expect, test } from "@playwright/test";
import { WASP_SERVER_URL } from "../playwright.config";
import { isRunningInDevMode } from "./helpers";

test.describe("server errors", () => {
  test("an unexpected error becomes a generic 500", async ({ request }) => {
    const response = await request.get(`${WASP_SERVER_URL}/errors/unexpected`);

    expect(response.status()).toBe(500);
    expect(await response.json()).toEqual({
      message: "Internal Server Error",
      ...expectedDevelopmentDetails(
        "Internal detail that must not reach the client",
      ),
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
    expect(await response.json()).toEqual({
      message: "Unauthorized",
      ...expectedDevelopmentDetails("Internal detail of a 401"),
    });
  });

  test("an unknown error keeps its status and headers but hides its message", async ({
    request,
  }) => {
    const response = await request.get(`${WASP_SERVER_URL}/errors/unavailable`);

    expect(response.status()).toBe(503);
    expect(response.headers()["retry-after"]).toBe("60");
    expect(
      response
        .headersArray()
        .filter(({ name }) => name.toLowerCase() === "set-cookie")
        .map(({ value }) => value),
    ).toEqual(["first=1; Path=/", "second=2; Path=/"]);
    expect(await response.json()).toEqual({
      message: "Service Unavailable",
      ...expectedDevelopmentDetails("Internal detail of a 503"),
    });
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

// In development, the server adds a hidden error's message and stack.
function expectedDevelopmentDetails(message: string) {
  if (!isRunningInDevMode()) {
    return {};
  }
  return {
    data: { message, stack: expect.stringContaining(message) },
  };
}
