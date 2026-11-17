import { HttpError, type HttpErrorData } from "./HttpError.js";

/**
 * The error the client gets when the server answers a request with an error
 * status.
 *
 * Failed Operations and auth functions (e.g. `login`) throw a
 * `ClientHttpError` with the `statusCode`, `message`, and `data` of the
 * [`ServerHttpError`](https://wasp.sh/docs/api/wasp/server#serverhttperror)
 * the server threw. For your custom APIs, pass the error that the `api` client
 * throws through
 * [`handleApiError`](https://wasp.sh/docs/api/wasp/client/api#handleapierror)
 * to get one.
 *
 * Requests can also fail without an answer from the server, e.g. when the
 * network is down. Those throw other errors, so check for a `ClientHttpError`
 * before reading its fields.
 *
 * Next to the fields it shares with the server's error, it has the `request`
 * and the `response` it comes from.
 *
 * You usually only read a `ClientHttpError`. Create one when you need to stand
 * in for the server, e.g. in tests.
 *
 * @typeParam Data The type of the error's `data`. It defaults to
 * {@link HttpErrorData}, an object with any fields.
 *
 * @example
 * ```ts
 * import { ClientHttpError } from "wasp/client";
 * import { createTask } from "wasp/client/operations";
 *
 * try {
 *   await createTask({ description: "Buy milk" });
 * } catch (error: unknown) {
 *   if (error instanceof ClientHttpError && error.statusCode === 403) {
 *     window.alert(error.message);
 *   }
 * }
 * ```
 *
 * @example
 * Reading a response header:
 *
 * ```ts
 * if (error instanceof ClientHttpError && error.statusCode === 429) {
 *   const retryAfter = error.response?.headers.get("Retry-After");
 * }
 * ```
 *
 * @see [Error handling](https://wasp.sh/docs/features/data/operations/queries#error-handling)
 */
export class ClientHttpError<
  Data extends HttpErrorData = HttpErrorData,
> extends HttpError<Data> {
  /**
   * The request the server answered with an error, from
   * {@link ClientHttpErrorOptions.request}. Use it to read the request's URL,
   * method, or headers.
   *
   * It is `undefined` if the error was created without it.
   */
  public request: Request | undefined;

  /**
   * The server's response, from {@link ClientHttpErrorOptions.response}. Use
   * it to read the response's headers or URL.
   *
   * Its body is already read, into `message` and `data`, so you can't read it
   * again. When the server is on another origin, the browser only shows the
   * response headers that the server lists in `Access-Control-Expose-Headers`.
   *
   * It is `undefined` if the error was created without it.
   */
  public response: Response | undefined;

  /**
   * @param statusCode The HTTP status code. It must be an integer in the
   * range `[400, 600)`, otherwise the constructor throws an `Error`.
   * @param message The error message.
   * @param data An object with any extra information about the error.
   * @param options The standard `Error` options (e.g. `cause`), and the
   * `request` and `response` the error comes from.
   */
  constructor(
    statusCode: number,
    message?: string,
    data?: Data,
    options?: ClientHttpErrorOptions,
  ) {
    super(statusCode, message, data, options);

    // We don't use `this.constructor.name` because minifiers rename classes.
    this.name = "ClientHttpError";
    this.request = options?.request;
    this.response = options?.response;
  }
}

/**
 * The options of a {@link ClientHttpError}: the standard `Error` options (e.g.
 * `cause`), and the `request` and `response` the error comes from.
 *
 * @example
 * ```ts
 * import { ClientHttpError } from "wasp/client";
 *
 * const error = new ClientHttpError(429, "Too many requests", undefined, {
 *   response: new Response(null, {
 *     status: 429,
 *     headers: { "Retry-After": "60" },
 *   }),
 * });
 * ```
 */
export interface ClientHttpErrorOptions extends ErrorOptions {
  /** The request the server answered with an error. */
  request?: Request;

  /** The server's response. */
  response?: Response;
}
