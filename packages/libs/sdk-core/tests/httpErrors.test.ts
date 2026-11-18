import { describe, expect, it } from "vitest";
import { ClientHttpError } from "../src/ClientHttpError";
import { HttpError, isHttpErrorBody, toHttpErrorBody } from "../src/HttpError";
import { ServerHttpError } from "../src/ServerHttpError";

describe("ServerHttpError", () => {
  it("is an `HttpError` with its status code, message, data, and headers", () => {
    const error = new ServerHttpError(
      429,
      "Too many requests",
      { limitPerMinute: 60 },
      { headers: { "Retry-After": 60 } },
    );

    expect(error).toBeInstanceOf(HttpError);
    expect(error).toMatchObject({
      name: "ServerHttpError",
      statusCode: 429,
      message: "Too many requests",
      data: { limitPerMinute: 60 },
      headers: { "Retry-After": 60 },
    });
  });

  it.each([200, 399, 600, 404.5])(
    "rejects the status code %d",
    (statusCode) => {
      expect(() => new ServerHttpError(statusCode)).toThrow(
        "statusCode has to be integer in range [400, 600).",
      );
    },
  );
});

describe("HttpError", () => {
  it("names classes that extend it without setting a name", () => {
    class PaymentRequiredError extends HttpError {
      constructor() {
        super(402, "Payment required");
      }
    }

    expect(new PaymentRequiredError().name).toBe("HttpError");
  });
});

describe("ClientHttpError", () => {
  it("is an `HttpError` with the request and response it comes from", () => {
    const request = new Request("http://localhost:3001/foo");
    const response = new Response(null, { status: 403 });

    const error = new ClientHttpError(403, "Not allowed", undefined, {
      request,
      response,
    });

    expect(error).toBeInstanceOf(HttpError);
    expect(error).toMatchObject({
      name: "ClientHttpError",
      statusCode: 403,
      message: "Not allowed",
      data: undefined,
      request,
      response,
    });
  });
});

describe("HttpErrorBody", () => {
  it("is built from an error's message and data", () => {
    const error = new ServerHttpError(403, "Not allowed", { reason: "owner" });

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
