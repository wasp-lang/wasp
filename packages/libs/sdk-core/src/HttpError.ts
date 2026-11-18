/**
 * An error for an HTTP error response, which has a status code in the range
 * `[400, 600)`.
 *
 * Its `statusCode`, `message`, and `data` are part of the response, so only
 * put information in them that is safe to show to the user.
 *
 * @typeParam Data The type of the error's `data`. It defaults to
 * {@link HttpErrorData}, an object with any fields.
 *
 * @example
 * ```ts
 * import { HttpError } from "wasp/server";
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
 * import { HttpError } from "wasp/server";
 *
 * throw new HttpError(
 *   429, // status code
 *   "Too many requests", // message
 *   { limitPerMinute: 60 }, // data
 *   { headers: { "Retry-After": "60" } }, // options
 * );
 * ```
 *
 * @example
 * Reading it on the client:
 *
 * ```ts
 * import { HttpError } from "wasp/client";
 * import { createTask } from "wasp/client/operations";
 *
 * try {
 *   await createTask({ description: "Buy milk" });
 * } catch (error: unknown) {
 *   if (error instanceof HttpError && error.statusCode === 429) {
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
  public get statusCode(): number {
    return this.response.status;
  }

  /**
   * @param statusCode The HTTP status code. It must be an integer in the
   * range `[400, 600)`, otherwise the constructor throws an `Error`.
   * @param message The error message.
   * @param data An object with any extra information about the error.
   * @param options The standard `Error` options (e.g. `cause`), and `headers`
   * for the response.
   */
  constructor(
    statusCode: number,
    message?: string,
    data?: Data,
    options?: HttpErrorOptions,
  ) {
    super(message, options);

    if (Error.captureStackTrace) {
      Error.captureStackTrace(this, new.target);
    }

    // We don't use `this.constructor.name` because minifiers rename classes.
    // Classes that extend `HttpError` set their own name.
    this.name = "HttpError";

    if (!isHttpErrorStatusCode(statusCode)) {
      throw new Error("statusCode has to be integer in range [400, 600).");
    }
    this.data = data;
    this.response =
      (options as InternalHttpErrorOptions | undefined)?.[existingResponse] ??
      Response.json(toHttpErrorBody(this), {
        status: statusCode,
        headers: options?.headers,
      });
  }

  /**
   * Creates the error for an HTTP error response that already exists, e.g.
   * one a client received.
   *
   * @param response The response. Its status code must be in the range
   * `[400, 600)`.
   * @param body The parsed body of the response.
   * @param options The standard `Error` options (e.g. `cause`).
   */
  static fromResponse(
    response: Response,
    { message, data }: HttpErrorBody,
    options?: ErrorOptions,
  ): HttpError {
    const internalOptions: InternalHttpErrorOptions = {
      ...options,
      [existingResponse]: response,
    };
    return new HttpError(response.status, message, data, internalOptions);
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
 * (e.g. `cause`), and `headers`.
 *
 * @example
 * ```ts
 * import { HttpError } from "wasp/server";
 *
 * try {
 *   await chargeCard(card);
 * } catch (error: unknown) {
 *   throw new HttpError(503, "Payments are unavailable", undefined, {
 *     cause: error,
 *     headers: { "Retry-After": "60" },
 *   });
 * }
 * ```
 */
export interface HttpErrorOptions extends ErrorOptions {
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

// Lets `fromResponse` hand the constructor a response to keep, without making
// that part of the public options.
const existingResponse = Symbol("existingResponse");
interface InternalHttpErrorOptions extends HttpErrorOptions {
  [existingResponse]?: Response;
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
