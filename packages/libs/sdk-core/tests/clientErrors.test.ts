import { HTTPError } from "ky";
import { beforeEach, describe, expect, it, vi } from "vitest";
import { reportClientError } from "../src/browser/errors";
import { HttpError } from "../src/HttpError";

describe("client error handler", () => {
  beforeEach(() => {
    vi.spyOn(console, "error").mockImplementation(() => {});
  });

  it("reports an unexpected error with its source", () => {
    const error = new Error("Unexpected");

    reportClientError(error, { source: "render" });

    expect(console.error).toHaveBeenCalledExactlyOnceWith(
      "Unexpected error in render:",
      error,
    );
  });

  it("doesn't report an `HttpError` with a status below 500", () => {
    reportClientError(new HttpError(404, "Not found"), {
      source: "window",
    });

    expect(console.error).not.toHaveBeenCalled();
  });

  it("reports an `HttpError` with a status of 500 or above", () => {
    const error = new HttpError(502, "Bad gateway");

    reportClientError(error, { source: "window" });

    expect(console.error).toHaveBeenCalledExactlyOnceWith(
      "Unexpected error in window:",
      error,
    );
  });

  it("doesn't report an HTTP error that ky threw with a status below 500", () => {
    reportClientError(makeKyHttpError(401), { source: "window" });

    expect(console.error).not.toHaveBeenCalled();
  });

  it("reports an HTTP error that ky threw with a status of 500 or above", () => {
    const kyHttpError = makeKyHttpError(503);

    reportClientError(kyHttpError, { source: "window" });

    expect(console.error).toHaveBeenCalledExactlyOnceWith(
      "Unexpected error in window:",
      kyHttpError,
    );
  });

  it("reports the same error object only once", () => {
    const error = new Error("Unexpected");

    reportClientError(error, { source: "render" });
    reportClientError(error, { source: "window" });

    expect(console.error).toHaveBeenCalledTimes(1);
  });
});

function makeKyHttpError(status: number): HTTPError {
  return new HTTPError(
    new Response(null, { status }),
    new Request("http://localhost:3001/foo"),
    // The handler only looks at the error's response.
    {} as ConstructorParameters<typeof HTTPError>[2],
  );
}
