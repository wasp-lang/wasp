import { beforeEach, describe, expect, it, vi } from "vitest";

describe("client error handler", () => {
  let HttpError: typeof import("../src/HttpError").HttpError;
  let KyHttpError: typeof import("ky").HTTPError;
  let reportClientError: typeof import("../src/browser/errors").reportClientError;
  let registerClientOnErrorFn: typeof import("../src/browser/errors").registerClientOnErrorFn;

  beforeEach(async () => {
    // The registered `client.onError` function lives in module state, so each
    // test needs fresh modules. The error classes must come from the same
    // module instances for `instanceof` checks to work.
    vi.resetModules();
    vi.spyOn(console, "error").mockImplementation(() => {});
    ({ HttpError } = await import("../src/HttpError"));
    ({ HTTPError: KyHttpError } = await import("ky"));
    ({ reportClientError, registerClientOnErrorFn } = await import(
      "../src/browser/errors"
    ));
  });

  it("logs an unexpected error with its source", () => {
    const error = new Error("Unexpected");

    reportClientError(error, { source: "render" });

    expect(console.error).toHaveBeenCalledExactlyOnceWith(
      "Unexpected error in render:",
      error,
    );
  });

  it("passes an unexpected error and its context to the `client.onError` function", () => {
    const onErrorFn = vi.fn();
    registerClientOnErrorFn(onErrorFn);
    const error = new Error("Unexpected");

    reportClientError(error, { source: "render" });

    expect(onErrorFn).toHaveBeenCalledExactlyOnceWith(error, {
      source: "render",
    });
  });

  it("doesn't report an `HttpError` with a status below 500", () => {
    const onErrorFn = vi.fn();
    registerClientOnErrorFn(onErrorFn);

    reportClientError(new HttpError(404, "Not found"), {
      source: "window",
    });

    expect(console.error).not.toHaveBeenCalled();
    expect(onErrorFn).not.toHaveBeenCalled();
  });

  it("reports an `HttpError` with a status of 500 or above", () => {
    const onErrorFn = vi.fn();
    registerClientOnErrorFn(onErrorFn);
    const error = new HttpError(502, "Bad gateway");

    reportClientError(error, { source: "window" });

    expect(onErrorFn).toHaveBeenCalledExactlyOnceWith(error, {
      source: "window",
    });
  });

  it("doesn't report an HTTP error that ky threw with a status below 500", () => {
    const onErrorFn = vi.fn();
    registerClientOnErrorFn(onErrorFn);

    reportClientError(makeKyHttpError(401), { source: "window" });

    expect(console.error).not.toHaveBeenCalled();
    expect(onErrorFn).not.toHaveBeenCalled();
  });

  it("reports an HTTP error that ky threw with a status of 500 or above", () => {
    const onErrorFn = vi.fn();
    registerClientOnErrorFn(onErrorFn);
    const kyHttpError = makeKyHttpError(503);

    reportClientError(kyHttpError, { source: "window" });

    expect(onErrorFn).toHaveBeenCalledExactlyOnceWith(kyHttpError, {
      source: "window",
    });
  });

  it("reports the same error object only once", () => {
    const onErrorFn = vi.fn();
    registerClientOnErrorFn(onErrorFn);
    const error = new Error("Unexpected");

    reportClientError(error, { source: "render" });
    reportClientError(error, { source: "window" });

    expect(onErrorFn).toHaveBeenCalledTimes(1);
  });

  it("survives a `client.onError` function that fails", async () => {
    registerClientOnErrorFn(async () => {
      throw new Error("Tracker is down");
    });

    expect(() =>
      reportClientError(new Error("Unexpected"), { source: "window" }),
    ).not.toThrow();
    await vi.waitFor(() =>
      expect(console.error).toHaveBeenCalledWith(
        "The `client.onError` function failed:",
        expect.any(Error),
      ),
    );
  });

  function makeKyHttpError(status: number): InstanceType<typeof KyHttpError> {
    return new KyHttpError(
      new Response(null, { status }),
      new Request("http://localhost:3001/foo"),
      // The handler only looks at the error's response.
      {} as ConstructorParameters<typeof KyHttpError>[2],
    );
  }
});
