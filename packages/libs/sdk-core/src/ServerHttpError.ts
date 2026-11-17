import { HttpError, type HttpErrorData } from "./HttpError.js";

/**
 * The error you throw on the server to send an HTTP error response to the
 * client.
 *
 * Its `statusCode`, `message`, and `data` are part of the response, so only
 * put information in them that is safe to show to the user.
 *
 * @typeParam Data The type of the error's `data`. It defaults to
 * {@link HttpErrorData}, an object with any fields.
 *
 * @example
 * ```ts
 * import { ServerHttpError } from "wasp/server";
 *
 * export const getTask = async ({ id }, context) => {
 *   if (!context.user) {
 *     throw new ServerHttpError(401, "You must be logged in.");
 *   }
 *   // ...
 * };
 * ```
 *
 * @example
 * Sending extra information and response headers:
 *
 * ```ts
 * import { ServerHttpError } from "wasp/server";
 *
 * throw new ServerHttpError(
 *   429, // status code
 *   "Too many requests", // message
 *   { limitPerMinute: 60 }, // data
 *   { headers: { "Retry-After": 60 } }, // options
 * );
 * ```
 *
 * @see [Error handling](https://wasp.sh/docs/features/data/operations/queries#error-handling)
 */
export class ServerHttpError<
  Data extends HttpErrorData = HttpErrorData,
> extends HttpError<Data> {
  /**
   * The headers to set on the error response, from
   * {@link ServerHttpErrorOptions.headers}.
   */
  public headers: ServerHttpErrorHeaders | undefined;

  /**
   * @param statusCode The HTTP status code. It must be an integer in the
   * range `[400, 600)`, otherwise the constructor throws an `Error`.
   * @param message The error message.
   * @param data An object with any extra information about the error.
   * @param options The standard `Error` options (e.g. `cause`), and `headers`
   * to set on the response.
   */
  constructor(
    statusCode: number,
    message?: string,
    data?: Data,
    options?: ServerHttpErrorOptions,
  ) {
    super(statusCode, message, data, options);

    // We don't use `this.constructor.name` because minifiers rename classes.
    this.name = "ServerHttpError";
    this.headers = options?.headers;
  }
}

/**
 * The options of a {@link ServerHttpError}. It takes the standard `Error`
 * options (e.g. `cause`), and `headers`.
 *
 * @example
 * ```ts
 * import { ServerHttpError } from "wasp/server";
 *
 * try {
 *   await chargeCard(card);
 * } catch (error: unknown) {
 *   throw new ServerHttpError(503, "Payments are unavailable", undefined, {
 *     cause: error,
 *     headers: { "Retry-After": 60 },
 *   });
 * }
 * ```
 */
export interface ServerHttpErrorOptions extends ErrorOptions {
  /**
   * Headers to set on the error response, e.g. `Retry-After`.
   */
  headers?: ServerHttpErrorHeaders;
}

/**
 * The response headers a {@link ServerHttpError} can set, passed through
 * {@link ServerHttpErrorOptions}.
 *
 * A value is a string, a number, or a list of strings for headers that
 * repeat, like `Set-Cookie`. These are the values that Node's
 * `response.setHeader` accepts.
 *
 * @example
 * ```ts
 * const headers: ServerHttpErrorHeaders = {
 *   "Retry-After": 60,
 *   "Set-Cookie": ["a=1", "b=2"],
 * };
 * ```
 */
export type ServerHttpErrorHeaders = {
  [headerName: string]: string | number | string[];
};
