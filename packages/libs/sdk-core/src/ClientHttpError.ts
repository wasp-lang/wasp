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
 * @see [Error handling](https://wasp.sh/docs/features/data/operations/queries#error-handling)
 */
export class ClientHttpError<
  Data extends HttpErrorData = HttpErrorData,
> extends HttpError<Data> {
  /**
   * @param statusCode The HTTP status code. It must be an integer in the
   * range `[400, 600)`, otherwise the constructor throws an `Error`.
   * @param message The error message.
   * @param data An object with any extra information about the error.
   * @param options The standard `Error` options (e.g. `cause`).
   */
  constructor(
    statusCode: number,
    message?: string,
    data?: Data,
    options?: ErrorOptions,
  ) {
    super(statusCode, message, data, options);

    // We don't use `this.constructor.name` because minifiers rename classes.
    this.name = "ClientHttpError";
  }
}
