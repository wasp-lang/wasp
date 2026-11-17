import { HttpError, type HttpErrorData } from "./HttpError.js";

/**
 * The error the client gets when the server answers a request with an HTTP
 * error status.
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
