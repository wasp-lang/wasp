/**
 * The error that the server and the client share. Throw it on the server to
 * send an HTTP error response to the client, and check for it on the client
 * to read that response.
 *
 * **On the server**, throw an `HttpError` from an Operation or API. Wasp
 * answers with its `statusCode`, and sends its `message` and `data` to the
 * client for every status code, so only put information in them that is safe
 * to show to the user.
 *
 * For security reasons, Wasp hides the details of any other error thrown while
 * handling a request: the client gets the generic message for the status code.
 * The status code is `500`, unless the error has a valid `status` or
 * `statusCode` field, in which case Wasp keeps it, along with the error's
 * `headers`. In development, Wasp also adds the original message and stack
 * trace to `data`. The only other errors that keep their message are the ones
 * from the `http-errors` package (which Express uses) with `expose` set to
 * `true`.
 *
 * **On the client**, failed Operations and auth functions (e.g. `login`) throw
 * an `HttpError` with the `statusCode`, `message`, and `data` you threw on the
 * server. For your custom APIs, pass the error that the `api` client throws
 * through `handleApiError` from `wasp/client/api` to get the same error.
 *
 * @typeParam Data The type of the error's `data`. It defaults to
 * {@link HttpErrorData}, an object with any fields.
 *
 * @example
 * Throwing it on the server:
 *
 * ```ts
 * import { HttpError } from "wasp/errors";
 *
 * export const getTask = async ({ id }, context) => {
 *   if (!context.user) {
 *     throw new HttpError(401, "You must be logged in.");
 *   }
 *   // ...
 * };
 * ```
 *
 * @example
 * Sending extra information and response headers:
 *
 * ```ts
 * import { HttpError } from "wasp/errors";
 *
 * throw new HttpError(
 *   429, // status code
 *   "Too many requests", // message
 *   { limitPerMinute: 60 }, // data
 *   { headers: { "Retry-After": 60 } }, // options
 * );
 * ```
 *
 * @example
 * Reading it on the client:
 *
 * ```ts
 * import { createTask } from "wasp/client/operations";
 * import { HttpError } from "wasp/errors";
 *
 * try {
 *   await createTask({ description: "Buy milk" });
 * } catch (error: unknown) {
 *   if (error instanceof HttpError && error.statusCode === 403) {
 *     window.alert(error.message);
 *   }
 * }
 * ```
 *
 * @see [Error handling](https://wasp.sh/docs/features/data/operations/queries#error-handling)
 */
export class HttpError<
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
   *
   * On the client, it is the `data` the server threw the error with, after a
   * round trip through JSON.
   */
  public data: Data | undefined;

  /**
   * The headers Wasp sets on the error response, from
   * {@link HttpErrorOptions.headers}.
   *
   * It is always `undefined` on the client, because the server doesn't send
   * them back as part of the error.
   */
  public headers: HttpErrorHeaders | undefined;

  /**
   * @param statusCode The HTTP status code. It must be an integer in the
   * range `[400, 600)`, otherwise the constructor throws an `Error`.
   * @param message The error message. Wasp sends it to the client.
   * @param data An object with any extra information about the error. Wasp
   * sends it to the client.
   * @param options The standard `Error` options (e.g. `cause`), and `headers`
   * to set on the response.
   */
  constructor(
    statusCode: number,
    message?: string,
    data?: Data,
    options?: HttpErrorOptions,
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

    this.headers = options?.headers;
  }
}

/**
 * The extra information an {@link HttpError} carries in its `data`: an object
 * with any fields you choose.
 *
 * Wasp sends it to the client as JSON, so use values that survive
 * `JSON.stringify`. For example, a `Date` arrives as a string.
 *
 * @example
 * ```ts
 * const data: HttpErrorData = { code: "TASK_LIMIT_REACHED", limit: 10 };
 * ```
 */
export type HttpErrorData = { [fieldName: string]: unknown };

/**
 * The options of an {@link HttpError}: the standard `Error` options (e.g.
 * `cause`), and `headers`.
 *
 * @example
 * ```ts
 * import { HttpError } from "wasp/errors";
 *
 * try {
 *   await chargeCard(card);
 * } catch (error: unknown) {
 *   throw new HttpError(503, "Payments are unavailable", undefined, {
 *     cause: error,
 *     headers: { "Retry-After": 60 },
 *   });
 * }
 * ```
 */
export interface HttpErrorOptions extends ErrorOptions {
  /**
   * Headers to set on the error response, e.g. `Retry-After`.
   *
   * Wasp sets them on the server. The client doesn't get them back on the
   * `HttpError` it receives, so put anything the client code needs in `data`.
   */
  headers?: HttpErrorHeaders;
}

/**
 * The response headers an {@link HttpError} can set, passed through
 * {@link HttpErrorOptions}.
 *
 * The values are the ones Node's `response.setHeader` accepts: a string, a
 * number, or a list of strings for headers that repeat, like `Set-Cookie`.
 *
 * @example
 * ```ts
 * const headers: HttpErrorHeaders = {
 *   "Retry-After": 60,
 *   "Set-Cookie": ["a=1", "b=2"],
 * };
 * ```
 */
export type HttpErrorHeaders = {
  [headerName: string]: string | number | string[];
};

/**
 * The JSON body the server sends for an `HttpError`.
 */
export type HttpErrorBody = {
  message: string;
  data?: HttpErrorData;
};

export function toHttpErrorBody(error: HttpError): HttpErrorBody {
  return { message: error.message, data: error.data };
}

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
