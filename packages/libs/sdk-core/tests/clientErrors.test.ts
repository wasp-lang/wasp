import { HTTPError } from "ky";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import {
  listenForUncaughtClientErrors,
  reportClientError,
  reportRenderError,
} from "../src/browser/errors";
import { HttpError } from "../src/HttpError";

describe("reportClientError", () => {
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
      event: "error",
    });

    expect(console.error).not.toHaveBeenCalled();
  });

  it("reports an `HttpError` with a status of 500 or above", () => {
    const error = new HttpError(502, "Bad gateway");

    reportClientError(error, { source: "window", event: "error" });

    expect(console.error).toHaveBeenCalledExactlyOnceWith(
      "Unexpected error in window:",
      error,
    );
  });

  it("doesn't report an HTTP error that ky threw with a status below 500", () => {
    reportClientError(makeKyHttpError(401), {
      source: "window",
      event: "error",
    });

    expect(console.error).not.toHaveBeenCalled();
  });

  it("reports an HTTP error that ky threw with a status of 500 or above", () => {
    const kyHttpError = makeKyHttpError(503);

    reportClientError(kyHttpError, { source: "window", event: "error" });

    expect(console.error).toHaveBeenCalledExactlyOnceWith(
      "Unexpected error in window:",
      kyHttpError,
    );
  });

  it("reports the same error object only once", () => {
    const error = new Error("Unexpected");

    reportClientError(error, { source: "render" });
    reportClientError(error, { source: "window", event: "error" });

    expect(console.error).toHaveBeenCalledTimes(1);
  });
});

describe("reportRenderError", () => {
  beforeEach(() => {
    vi.spyOn(console, "error").mockImplementation(() => {});
  });

  it("reports the error as a render error with its component stack", () => {
    const error = new Error("Thrown while rendering");

    reportRenderError(error, { componentStack: "\n    at Page" });

    expect(console.error).toHaveBeenCalledWith(
      "Unexpected error in render:",
      error,
    );
    expect(console.error).toHaveBeenCalledWith(
      "The error happened in:\n    at Page",
    );
  });
});

describe("listenForUncaughtClientErrors", () => {
  beforeEach(() => {
    vi.spyOn(console, "error").mockImplementation(() => {});
    vi.stubGlobal("window", new EventTarget());
  });

  afterEach(() => {
    vi.unstubAllGlobals();
  });

  it("reports the error of an `error` event", () => {
    const error = new Error("Thrown in an event handler");

    listenForUncaughtClientErrors();
    window.dispatchEvent(Object.assign(new Event("error"), { error }));

    expect(console.error).toHaveBeenCalledExactlyOnceWith(
      "Unexpected error in window:",
      error,
    );
  });

  it("reports the reason of an `unhandledrejection` event", () => {
    const reason = new Error("Rejected without a catch");

    listenForUncaughtClientErrors();
    window.dispatchEvent(
      Object.assign(new Event("unhandledrejection"), { reason }),
    );

    expect(console.error).toHaveBeenCalledExactlyOnceWith(
      "Unexpected error in window:",
      reason,
    );
  });
});

function makeKyHttpError(status: number): HTTPError {
  return new HTTPError(
    new Response(null, { status }),
    new Request("http://localhost:3001/foo"),
    // `reportClientError` only reads the error's response.
    {} as ConstructorParameters<typeof HTTPError>[2],
  );
}
