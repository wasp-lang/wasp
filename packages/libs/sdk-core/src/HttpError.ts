/**
 * An error for an HTTP error response, which has a status code in the range
 * `[400, 600)`.
 *
 * Its `status`, `message`, and `data` are part of the response, so only put
 * information in them that is safe to show to the user.
 *
 * @typeParam Data The type of the error's `data`. It defaults to
 * {@link HttpErrorData}, an object with any fields.
 *
 * @example
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
 * throw new HttpError(429, "Too many requests", {
 *   data: { limitPerMinute: 60 },
 *   headers: { "Retry-After": "60" },
 * });
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
 *   if (error instanceof HttpError && error.status === 429) {
 *     const retryAfter = error.response.headers.get("Retry-After");
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
   * The HTTP response the error stands for. Read the status code and the
   * headers from it.
   *
   * Don't read its body, since it may already be consumed. Use `message` and
   * `data` instead.
   */
  public readonly response: Response;

  /**
   * Extra information about the error, e.g. which form field is invalid.
   * It is `undefined` if the error was created without it.
   */
  public readonly data: Data | undefined;

  /**
   * The HTTP status code of the response, an integer in the range
   * `[400, 600)`.
   */
  public get status(): number {
    return this.response.status;
  }

  /**
   * @deprecated Use {@link HttpError.status | status} instead.
   */
  public get statusCode(): number {
    return this.status;
  }

  /**
   * Creates the error together with its response.
   *
   * @param status The HTTP status code. It must be an integer in the range
   * `[400, 600)`, otherwise the constructor throws an `Error`.
   * @param message The error message.
   * @param options The data, the headers for the response, and the standard
   * `Error` options (e.g. `cause`).
   */
  constructor(
    status: number,
    message?: string,
    options?: HttpErrorOptions<Data>,
  );
  /**
   * Creates the error for an existing response, e.g. one a client received.
   *
   * @param response The response the error stands for. Its status must be in
   * the range `[400, 600)`, otherwise the constructor throws an `Error`.
   * @param message The error message.
   * @param options The data and the standard `Error` options (e.g. `cause`).
   *
   * @example
   * ```ts
   * import { HttpError } from "wasp/errors";
   *
   * const response = await fetch("https://api.example.com/tasks");
   * if (!response.ok) {
   *   throw new HttpError(response, "Could not load the tasks.");
   * }
   * ```
   */
  constructor(
    response: Response,
    message?: string,
    options?: Omit<HttpErrorOptions<Data>, "headers">,
  );
  constructor(
    statusOrResponse: number | Response,
    message?: string,
    options: HttpErrorOptions<Data> = {},
  ) {
    super(message, options);

    if (Error.captureStackTrace) {
      Error.captureStackTrace(this, new.target);
    }

    // We don't use `this.constructor.name` because minifiers rename classes.
    // A class that extends `HttpError` can set its own name.
    this.name = "HttpError";

    const status =
      typeof statusOrResponse === "number"
        ? statusOrResponse
        : statusOrResponse.status;
    if (!isHttpErrorStatusCode(status)) {
      throw new Error(
        "The status of an HttpError has to be an integer in the range [400, 600).",
      );
    }

    this.data = options.data;
    this.response =
      typeof statusOrResponse === "number"
        ? Response.json(toHttpErrorBody(this), {
            status,
            headers: options.headers,
          })
        : statusOrResponse;
  }
}

/**
 * The extra information an {@link HttpError} carries in its `data`. It is an
 * object with any fields you choose.
 *
 * @example
 * ```ts
 * const data: HttpErrorData = { code: "TASK_LIMIT_REACHED", limit: 10 };
 * ```
 */
export type HttpErrorData = { [fieldName: string]: unknown };

/**
 * The options of an {@link HttpError}. It takes the standard `Error` options
 * (e.g. `cause`), the data, and the headers of the error's response.
 *
 * @typeParam Data The type of `data`.
 *
 * @example
 * ```ts
 * import { HttpError } from "wasp/errors";
 *
 * try {
 *   await chargeCard(card);
 * } catch (error: unknown) {
 *   throw new HttpError(503, "Payments are unavailable", {
 *     headers: { "Retry-After": "60" },
 *     cause: error,
 *   });
 * }
 * ```
 */
export interface HttpErrorOptions<Data extends HttpErrorData = HttpErrorData>
  extends ErrorOptions {
  /** An object with any extra information about the error. */
  data?: Data;

  /**
   * Headers for the error response, e.g. `Retry-After`. To repeat a header,
   * like `Set-Cookie`, pass a list of name and value pairs.
   *
   * @example
   * ```ts
   * const headers: HeadersInit = [
   *   ["Set-Cookie", "a=1"],
   *   ["Set-Cookie", "b=2"],
   * ];
   * ```
   */
  headers?: HeadersInit;
}

/**
 * Checks that a value is a status code an {@link HttpError} accepts, which is
 * an integer in the range `[400, 600)`.
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
 * The JSON body of the response of an {@link HttpError}.
 */
export type HttpErrorBody = {
  message: string;
  data?: HttpErrorData;
};

/**
 * Builds the JSON body of the response of an {@link HttpError}.
 */
export function toHttpErrorBody(error: HttpError): HttpErrorBody {
  return { message: error.message, data: error.data };
}

/**
 * Checks that a parsed response body is an {@link HttpErrorBody}. It isn't
 * when something other than the Wasp server answered, e.g. a proxy.
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
