/**
 * The base form of Wasp's HTTP errors. It holds what the server's and the
 * client's errors have in common: the status code, the message, and the data.
 *
 * It is abstract, so you create one of the classes that extend it. Use
 * `HttpError` itself to handle any of them, e.g. in code that runs on both the
 * server and the client.
 *
 * @typeParam Data The type of the error's `data`. It defaults to
 * {@link HttpErrorData}, an object with any fields.
 *
 * @example
 * ```ts
 * import { HttpError } from "wasp/errors";
 *
 * export function isNotFoundError(error: unknown): boolean {
 *   return error instanceof HttpError && error.statusCode === 404;
 * }
 * ```
 *
 * @see [Error handling](https://wasp.sh/docs/features/data/operations/queries#error-handling)
 */
export abstract class HttpError<
  Data extends HttpErrorData = HttpErrorData,
> extends Error {
  /**
   * The HTTP status code of the response, an integer in the range
   * `[400, 600)`.
   */
  public statusCode: number;

  /**
   * Extra information about the error, e.g. which form field is invalid.
   * It is `undefined` if the error was created without it.
   */
  public data: Data | undefined;

  /**
   * @param statusCode The HTTP status code. It must be an integer in the
   * range `[400, 600)`, otherwise the constructor throws an `Error`.
   * @param message The error message.
   * @param data An object with any extra information about the error.
   * @param options The standard `Error` options (e.g. `cause`).
   */
  protected constructor(
    statusCode: number,
    message?: string,
    data?: Data,
    options?: ErrorOptions,
  ) {
    super(message, options);

    if (Error.captureStackTrace) {
      Error.captureStackTrace(this, new.target);
    }

    if (!isHttpErrorStatusCode(statusCode)) {
      throw new Error("statusCode has to be integer in range [400, 600).");
    }
    this.statusCode = statusCode;

    if (data) {
      this.data = data;
    }
  }
}

/**
 * The extra information an {@link HttpError} carries in its `data`: an object
 * with any fields you choose.
 *
 * It travels as JSON, so use values that survive `JSON.stringify`. For
 * example, a `Date` becomes a string.
 *
 * @example
 * ```ts
 * const data: HttpErrorData = { code: "TASK_LIMIT_REACHED", limit: 10 };
 * ```
 */
export type HttpErrorData = { [fieldName: string]: unknown };

/**
 * Checks for a status code an {@link HttpError} accepts: an integer in the
 * range `[400, 600)`.
 */
export function isHttpErrorStatusCode(value: unknown): value is number {
  return (
    typeof value === "number" &&
    Number.isInteger(value) &&
    value >= 400 &&
    value < 600
  );
}

/**
 * The JSON body the server sends for an {@link HttpError}.
 */
export type HttpErrorBody = {
  message: string;
  data?: HttpErrorData;
};

/**
 * Builds the JSON body the server sends for an {@link HttpError}.
 */
export function toHttpErrorBody(error: HttpError): HttpErrorBody {
  return { message: error.message, data: error.data };
}

/**
 * Checks that a response body is the one the server sends for an
 * {@link HttpError}. It isn't when something else answered, e.g. a proxy.
 */
export function isHttpErrorBody(body: unknown): body is HttpErrorBody {
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
