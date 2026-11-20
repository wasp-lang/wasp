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

  test("an unexpected error reaches the `server.onError` function", async ({
    request,
  }) => {
    await request.get(`${WASP_SERVER_URL}/errors/unexpected`);

    const response = await request.get(`${WASP_SERVER_URL}/errors/reported`);
    expect(await response.json()).toContainEqual({
      source: "request",
      message: "Internal detail that must not reach the client",
    });
  });

  test("an HttpError keeps its status, headers, message, and data", async ({
    request,
  }) => {
    const response = await request.get(`${WASP_SERVER_URL}/errors/http`);

    expect(response.status()).toBe(418);
    expect(response.headers()["content-type"]).toContain(
      "application/vnd.teapot+json",
    );
    expect(await response.json()).toEqual({
      message: "I'm a teapot",
      data: { reason: "short and stout" },
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
