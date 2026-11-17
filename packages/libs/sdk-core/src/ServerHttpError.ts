import { HttpError, type HttpErrorData } from "./HttpError.js";

/**
 * The error you throw on the server to send an HTTP error response to the
 * client.
 *
 * Throw a `ServerHttpError` from an Operation or API. Wasp answers with its
 * `statusCode`, and sends its `message` and `data` to the client for every
 * status code, so only put information in them that is safe to show to the
 * user. On the client, the failed request throws a
 * [`ClientHttpError`](https://wasp.sh/docs/api/wasp/client#clienthttperror)
 * with the same `statusCode`, `message`, and `data`.
 *
 * For security reasons, Wasp hides the details of any error that isn't an
 * `HttpError` and is thrown while handling a request: the client gets the generic message for the status code.
 * The status code is `500`, unless the error has a valid `status` or
 * `statusCode` field, in which case Wasp keeps it, along with the error's
 * `headers`. In development, Wasp also adds the original message and stack
 * trace to `data`. The only other errors that keep their message are the ones
 * from the `http-errors` package (which Express uses) with `expose` set to
 * `true`.
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
   * The headers Wasp sets on the error response, from
   * {@link ServerHttpErrorOptions.headers}.
   */
  public headers: ServerHttpErrorHeaders | undefined;

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
    options?: ServerHttpErrorOptions,
  ) {
    super(statusCode, message, data, options);

    // We don't use `this.constructor.name` because minifiers rename classes.
    this.name = "ServerHttpError";
    this.headers = options?.headers;
  }
}

/**
 * The options of a {@link ServerHttpError}: the standard `Error` options (e.g.
 * `cause`), and `headers`.
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
   *
   * The client's error doesn't have them as a field. Client code can read
   * them from the error's `response`, but only the ones the browser is allowed
   * to see, so put anything the client code needs in `data`.
   */
  headers?: ServerHttpErrorHeaders;
}

/**
 * The response headers a {@link ServerHttpError} can set, passed through
 * {@link ServerHttpErrorOptions}.
 *
 * The values are the ones Node's `response.setHeader` accepts: a string, a
 * number, or a list of strings for headers that repeat, like `Set-Cookie`.
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
