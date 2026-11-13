import { HTTPError } from "ky";
import { beforeEach, describe, expect, it, vi } from "vitest";
import { handleClientError } from "../src/browser/errors";
import { HttpError } from "../src/HttpError";

describe("client error handler", () => {
  beforeEach(() => {
    vi.spyOn(console, "error").mockImplementation(() => {});
  });

  it("reports an unexpected error with its source", () => {
    const error = new Error("Unexpected");

    handleClientError(error, { source: "render" });

    expect(console.error).toHaveBeenCalledExactlyOnceWith(
      "Unexpected error in render:",
      error,
    );
  });

  it("doesn't report an `HttpError`", () => {
    handleClientError(new HttpError(404, "Not found"), { source: "query" });

    expect(console.error).not.toHaveBeenCalled();
  });

  it("doesn't report an HTTP error that ky threw", () => {
    const kyHttpError = new HTTPError(
      new Response(null, { status: 401 }),
      new Request("http://localhost:3001/foo"),
      // The handler only looks at the error's type.
      {} as ConstructorParameters<typeof HTTPError>[2],
    );

    handleClientError(kyHttpError, { source: "query" });

    expect(console.error).not.toHaveBeenCalled();
  });

  it("reports the same error object only once", () => {
    const error = new Error("Unexpected");

    handleClientError(error, { source: "action" });
    handleClientError(error, { source: "window" });

    expect(console.error).toHaveBeenCalledTimes(1);
  });
});
