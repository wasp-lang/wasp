import { describe, expect, it } from "vitest";
import { HttpError, isHttpErrorBody, toHttpErrorBody } from "../src/HttpError";

describe("HttpError", () => {
  it("is an `Error` with its status code, message, and data", () => {
    const cause = new Error("Upstream failed");
    const error = new HttpError(
      429,
      "Too many requests",
      { limitPerMinute: 60 },
      { cause },
    );

    expect(error).toBeInstanceOf(Error);
    expect(error).toMatchObject({
      name: "HttpError",
      statusCode: 429,
      message: "Too many requests",
      data: { limitPerMinute: 60 },
      cause,
    });
  });

  it("builds the response it stands for", async () => {
    const error = new HttpError(
      429,
      "Too many requests",
      { limitPerMinute: 60 },
      {
        headers: [
          ["Retry-After", "60"],
          ["Set-Cookie", "a=1"],
          ["Set-Cookie", "b=2"],
        ],
      },
    );

    expect(error.response.status).toBe(429);
    expect(error.response.headers.get("Retry-After")).toBe("60");
    expect(error.response.headers.getSetCookie()).toEqual(["a=1", "b=2"]);
    expect(await error.response.json()).toEqual({
      message: "Too many requests",
      data: { limitPerMinute: 60 },
    });
  });

  it.each([200, 399, 600, 404.5])(
    "rejects the status code %d",
    (statusCode) => {
      expect(() => new HttpError(statusCode)).toThrow(
        "statusCode has to be integer in range [400, 600).",
      );
    },
  );

  it("names classes that extend it without setting a name", () => {
    class PaymentRequiredError extends HttpError {
      constructor() {
        super(402, "Payment required");
      }
    }

    expect(new PaymentRequiredError().name).toBe("HttpError");
  });

  it("keeps the response it was created from", () => {
    const response = new Response(null, {
      status: 403,
      headers: { "X-Reason": "owner" },
    });

    const error = HttpError.fromResponse(response, {
      message: "Not allowed",
      data: { reason: "owner" },
    });

    expect(error.response).toBe(response);
    expect(error).toMatchObject({
      statusCode: 403,
      message: "Not allowed",
      data: { reason: "owner" },
    });
  });
});

describe("HttpErrorBody", () => {
  it("is built from an error's message and data", () => {
    const error = new HttpError(403, "Not allowed", { reason: "owner" });

    expect(toHttpErrorBody(error)).toEqual({
      message: "Not allowed",
      data: { reason: "owner" },
    });
  });

  it.each([
    { message: "Not allowed" },
    { message: "Not allowed", data: { reason: "owner" } },
  ])("accepts %j", (body) => {
    expect(isHttpErrorBody(body)).toBe(true);
  });

  it.each([
    undefined,
    null,
    "Bad Gateway",
    {},
    { message: 1 },
    { message: "x", data: 1 },
  ])("rejects %j", (body) => {
    expect(isHttpErrorBody(body)).toBe(false);
  });
});
