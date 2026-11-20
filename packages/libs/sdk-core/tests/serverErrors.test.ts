import type { Response } from "express";
import { beforeEach, describe, expect, it, vi } from "vitest";
import { HttpError } from "../src/HttpError";
import { reportUnexpectedServerError } from "../src/node/errors/report";
import { sendErrorResponse } from "../src/node/errors/response";
import { listenForUncaughtServerErrors } from "../src/node/errors/uncaught";
import { toHeadersInit } from "../src/node/errors/utils";

describe("sendErrorResponse", () => {
  beforeEach(() => {
    vi.spyOn(console, "error").mockImplementation(() => {});
  });

  it("sends an `HttpError` as is", () => {
    const sent = send(
      new HttpError(429, "Too many requests", {
        data: { limitPerMinute: 60 },
        headers: { "Retry-After": "60" },
      }),
    );

    expect(sent.status).toBe(429);
    expect(sent.headers["retry-after"]).toBe("60");
    expect(sent.body).toEqual({
      message: "Too many requests",
      data: { limitPerMinute: 60 },
    });
    expect(console.error).not.toHaveBeenCalled();
  });

  it("sends an error of a class that extends `HttpError` as is", () => {
    class PaymentRequiredError extends HttpError<{ plan: string }> {
      constructor(plan: string) {
        super(402, "Payment required", { data: { plan } });
      }
    }

    const sent = send(new PaymentRequiredError("pro"));

    expect(sent.status).toBe(402);
    expect(sent.body).toEqual({
      message: "Payment required",
      data: { plan: "pro" },
    });
  });

  it("keeps the content type that an `HttpError` sets", () => {
    const sent = send(
      new HttpError(418, "I'm a teapot", {
        headers: { "Content-Type": "application/vnd.teapot+json" },
      }),
    );

    expect(sent.headers["content-type"]).toBe("application/vnd.teapot+json");
  });

  it("sends each `Set-Cookie` value of an `HttpError` separately", () => {
    const sent = send(
      new HttpError(401, "Unauthorized", {
        headers: [
          ["Set-Cookie", "a=1"],
          ["Set-Cookie", "b=2"],
        ],
      }),
    );

    expect(sent.headers["Set-Cookie"]).toEqual(["a=1", "b=2"]);
  });

  it("keeps the message of an `http-errors` error that exposes it", () => {
    const sent = send(makeHttpErrorsError(400, "Bad JSON", { expose: true }));

    expect(sent.status).toBe(400);
    expect(sent.body).toEqual({ message: "Bad JSON" });
    expect(console.error).not.toHaveBeenCalled();
  });

  it("hides the message of an `http-errors` error that doesn't expose it", () => {
    const sent = send(
      makeHttpErrorsError(503, "The database is down", { expose: false }),
    );

    expect(sent.status).toBe(503);
    expect(sent.body).toEqual({ message: "Service Unavailable" });
  });

  it("keeps the status and the headers of an unknown error, and hides its message", () => {
    const error = Object.assign(new Error("Slow down, internal detail"), {
      status: 429,
      headers: { "Retry-After": 120 },
    });

    const sent = send(error);

    expect(sent.status).toBe(429);
    expect(sent.headers["retry-after"]).toBe("120");
    expect(sent.body).toEqual({ message: "Too Many Requests" });
  });

  it("doesn't forward the headers of an unknown error when they are a `Headers` instance", () => {
    const error = Object.assign(new Error("Upstream failed"), {
      status: 502,
      headers: new Headers({ "X-Upstream": "1" }),
    });

    const sent = send(error);

    expect(sent.status).toBe(502);
    expect(sent.headers).not.toHaveProperty("x-upstream");
  });

  it.each([200, 600, "404", undefined])(
    "answers with 500 for an unknown error with the status %s",
    (status) => {
      const sent = send(Object.assign(new Error("Unexpected"), { status }));

      expect(sent.status).toBe(500);
      expect(sent.body).toEqual({ message: "Internal Server Error" });
    },
  );

  it("answers with 500 for a thrown value that isn't an error", () => {
    expect(send("a string").status).toBe(500);
  });

  it("reports an unexpected error that ends with a status of 500 or above", () => {
    const error = new Error("Unexpected");

    send(error);

    expect(console.error).toHaveBeenCalledExactlyOnceWith(
      "Unexpected error in request:",
      error,
    );
  });

  it("includes the details of an unexpected error when asked to", () => {
    const error = new Error("Internal detail");

    const sent = send(error, { includeErrorDetails: true });

    expect(sent.body).toEqual({
      message: "Internal Server Error",
      data: { message: "Internal detail", stack: error.stack },
    });
  });
});

describe("toHeadersInit", () => {
  it("gives one pair for each value of a repeated header", () => {
    expect(
      toHeadersInit({ "Set-Cookie": ["a=1", "b=2"], "Retry-After": 60 }),
    ).toEqual([
      ["Set-Cookie", "a=1"],
      ["Set-Cookie", "b=2"],
      ["Retry-After", "60"],
    ]);
  });

  it("gives no pairs without headers", () => {
    expect(toHeadersInit(undefined)).toEqual([]);
  });
});

describe("reportUnexpectedServerError", () => {
  it("logs the error with its source and the details of its context", () => {
    vi.spyOn(console, "error").mockImplementation(() => {});
    const error = new Error("Unexpected");

    reportUnexpectedServerError(error, { source: "job", jobName: "sendEmail" });

    expect(console.error).toHaveBeenCalledExactlyOnceWith(
      "Unexpected error in job sendEmail:",
      error,
    );
  });
});

describe("listenForUncaughtServerErrors", () => {
  beforeEach(() => {
    vi.spyOn(console, "error").mockImplementation(() => {});
  });

  it("reports an uncaught exception and exits", () => {
    const exit = vi.spyOn(process, "exit").mockImplementation(() => {
      return undefined as never;
    });
    const listeners = collectProcessListeners();
    const error = new Error("Thrown in a timer");

    listenForUncaughtServerErrors();
    listeners.uncaughtException?.(error);

    expect(console.error).toHaveBeenCalledExactlyOnceWith(
      "Unexpected error in process uncaughtException:",
      error,
    );
    expect(exit).toHaveBeenCalledExactlyOnceWith(1);
  });

  it("reports an unhandled rejection and keeps running", () => {
    const exit = vi.spyOn(process, "exit").mockImplementation(() => {
      return undefined as never;
    });
    const listeners = collectProcessListeners();
    const reason = new Error("Rejected without a catch");

    listenForUncaughtServerErrors();
    listeners.unhandledRejection?.(reason);

    expect(console.error).toHaveBeenCalledExactlyOnceWith(
      "Unexpected error in process unhandledRejection:",
      reason,
    );
    expect(exit).not.toHaveBeenCalled();
  });
});

type ProcessListeners = { [eventName: string]: (error: unknown) => void };

// Collects the listeners instead of registering them, so a test can't change
// how the test process itself reacts to errors.
function collectProcessListeners(): ProcessListeners {
  const listeners: ProcessListeners = {};
  vi.spyOn(process, "on").mockImplementation(((
    eventName: string,
    listener: (error: unknown) => void,
  ) => {
    listeners[eventName] = listener;
    return process;
  }) as typeof process.on);
  return listeners;
}

type SentResponse = {
  status: number | undefined;
  headers: { [headerName: string]: unknown };
  body: unknown;
};

function send(
  error: unknown,
  options: { includeErrorDetails: boolean } = { includeErrorDetails: false },
): SentResponse {
  const sent: SentResponse = {
    status: undefined,
    headers: {},
    body: undefined,
  };
  const response = {
    set(name: string, value: unknown) {
      sent.headers[name] = value;
      return response;
    },
    status(status: number) {
      sent.status = status;
      return response;
    },
    json(body: unknown) {
      sent.body = body;
      return response;
    },
  };
  sendErrorResponse(response as unknown as Response, error, options);
  return sent;
}

function makeHttpErrorsError(
  statusCode: number,
  message: string,
  { expose }: { expose: boolean },
): Error {
  return Object.assign(new Error(message), {
    status: statusCode,
    statusCode,
    expose,
  });
}
