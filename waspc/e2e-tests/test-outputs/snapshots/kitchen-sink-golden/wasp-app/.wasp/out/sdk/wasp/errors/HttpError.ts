type HttpErrorData = Record<string, unknown>;

// PUBLIC API
/**
 * An error with an HTTP status code that Wasp sends to the client.
 *
 * Throw it on the server to answer with a specific status code, message,
 * and data. The client receives the same fields in an `HttpError`.
 * Anything else thrown on the server reaches the client as a generic 500.
 */
export class HttpError<
  Data extends HttpErrorData = HttpErrorData,
> extends Error {
  public statusCode: number;
  public data: Data | undefined;

  constructor(
    statusCode: number,
    message?: string,
    data?: Data,
    options?: ErrorOptions,
  ) {
    super(message, options);

    if (Error.captureStackTrace) {
      Error.captureStackTrace(this, HttpError);
    }

    // We don't use `this.constructor.name` because minifiers rename classes.
    this.name = "HttpError";

    if (
      !(Number.isInteger(statusCode) && statusCode >= 400 && statusCode < 600)
    ) {
      throw new Error("statusCode has to be integer in range [400, 600).");
    }
    this.statusCode = statusCode;

    if (data) {
      this.data = data;
    }
  }
}

// PRIVATE API (SDK)
/**
 * The JSON body the server sends for an `HttpError`.
 */
export type HttpErrorBody = {
  message: string;
  data?: HttpErrorData;
};

// PRIVATE API (server)
export function toHttpErrorBody(error: HttpError): HttpErrorBody {
  return { message: error.message, data: error.data };
}

// PRIVATE API (client)
/**
 * Rebuilds the `HttpError` the server sent. Uses `fallbackMessage` when the
 * response body isn't an `HttpErrorBody`, e.g. when a proxy answered.
 */
export function fromHttpErrorBody({
  statusCode,
  body,
  fallbackMessage,
}: {
  statusCode: number;
  body: unknown;
  fallbackMessage: string;
}): HttpError {
  if (!isHttpErrorBody(body)) {
    return new HttpError(statusCode, fallbackMessage);
  }
  return new HttpError(statusCode, body.message, body.data);
}

function isHttpErrorBody(body: unknown): body is HttpErrorBody {
  return (
    typeof body === "object" &&
    body !== null &&
    "message" in body &&
    typeof body.message === "string" &&
    (!("data" in body) || isHttpErrorData(body.data))
  );
}

function isHttpErrorData(data: unknown): data is HttpErrorData | undefined {
  return data === undefined || (typeof data === "object" && data !== null);
}
