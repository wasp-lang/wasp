import { describe, expect, it } from "vitest";
import { HttpError, isHttpErrorBody, toHttpErrorBody } from "../src/HttpError";

describe("HttpError", () => {
  it("is an `Error` with its status code, message, and data", () => {
    const cause = new Error("Upstream failed");
    const error = new HttpError(429, "Too many requests", {
      data: { limitPerMinute: 60 },
      cause,
    });

    expect(error).toBeInstanceOf(Error);
    expect(error).toMatchObject({
      name: "HttpError",
      status: 429,
      message: "Too many requests",
      data: { limitPerMinute: 60 },
      cause,
    });
  });

  it("builds the response it stands for", async () => {
    const error = new HttpError(429, "Too many requests", {
      data: { limitPerMinute: 60 },
      headers: [
        ["Retry-After", "60"],
        ["Set-Cookie", "a=1"],
        ["Set-Cookie", "b=2"],
      ],
    });

    expect(error.response.status).toBe(429);
    expect(error.response.headers.get("Retry-After")).toBe("60");
    expect(error.response.headers.getSetCookie()).toEqual(["a=1", "b=2"]);
    expect(await error.response.json()).toEqual({
      message: "Too many requests",
      data: { limitPerMinute: 60 },
    });
  });

  it.each([200, 399, 600, 404.5])("rejects the status code %d", (status) => {
    expect(() => new HttpError(status)).toThrow(
      "status has to be an integer in the range [400, 600).",
    );
  });

  it("rejects data passed in place of the options", () => {
    expect(
      () =>
        // @ts-expect-error The third argument is the options, not the data.
        new HttpError(403, "Not allowed", { reason: "owner" }),
    ).toThrow('Unknown HttpError option "reason".');
  });

  it("still has its status as `statusCode`", () => {
    expect(new HttpError(404).statusCode).toBe(404);
  });

  it("names classes that extend it without setting a name", () => {
    class PaymentRequiredError extends HttpError {
      constructor() {
        super(402, "Payment required");
      }
    }

    expect(new PaymentRequiredError().name).toBe("HttpError");
  });

  it("keeps a response passed to it", () => {
    const response = new Response(null, {
      status: 403,
      headers: { "X-Reason": "owner" },
    });

    const error = new HttpError(403, "Not allowed", {
      data: { reason: "owner" },
      response,
    });

    expect(error.response).toBe(response);
    expect(error).toMatchObject({
      status: 403,
      message: "Not allowed",
      data: { reason: "owner" },
    });
  });

  it("takes its status and headers from a response passed to it", () => {
    const response = new Response(null, {
      status: 404,
      headers: { "X-From": "response" },
    });

    const error = new HttpError(403, "Not allowed", {
      response,
      headers: { "X-From": "option" },
    });

    expect(error.status).toBe(404);
    expect(error.response.headers.get("X-From")).toBe("response");
  });

  it("rejects a response whose status isn't an error status", () => {
    const response = new Response(null, { status: 200 });

    expect(() => new HttpError(404, "Not found", { response })).toThrow(
      "The status of the response has to be in the range [400, 600).",
    );
  });
});

describe("HttpErrorBody", () => {
  it("is built from an error's message and data", () => {
    const error = new HttpError(403, "Not allowed", {
      data: { reason: "owner" },
    });

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
