/**
 * Errors that the server and the client share.
 *
 * You can import `wasp/errors` in both server and client code.
 *
 * @module
 */

/**
 * Throw an `HttpError` from an Operation or API to send an HTTP error response
 * to the client.
 *
 * For security reasons, Wasp hides the details of any other exception thrown
 * while handling a request: the client gets the generic message for the status
 * code. The status code is `500`, unless the error has a valid `status` or
 * `statusCode` field, in which case Wasp keeps it, along with the error's
 * `headers`. In development, Wasp also adds the original message and stack
 * trace to `data`. The only other errors that keep their message are the ones
 * from the `http-errors` package (which Express uses) with `expose` set to
 * `true`.
 *
 * The constructor takes:
 *
 * - `statusCode`: The HTTP status code. It must be an integer in the range
 *   `[400, 600)`.
 * - `message` (optional): The error message.
 * - `data` (optional): An object with any extra information about the error.
 * - `options` (optional): The standard `Error` options (e.g. `cause`), and
 *   `headers` to set on the response.
 *
 * Wasp sends `message` and `data` to the client for every status code, so only
 * put information in them that is safe to show to the user.
 *
 * The client gets the same error: failed Operations and auth functions (e.g.
 * `login`) throw an `HttpError` with the `statusCode`, `message`, and `data`
 * you threw on the server. For your custom APIs, pass the error that the `api`
 * client throws through `handleApiError` from `wasp/client/api` to get the
 * same error.
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
 * ```ts
 * import { HttpError } from "wasp/errors";
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
export { HttpError } from "@wasp/lib-sdk-core";

/**
 * Returns the message of anything that was thrown, e.g. the `error` of a
 * `catch` block, which TypeScript types as `unknown`.
 *
 * @param error The thrown value.
 * @returns The `message` of an `Error`, otherwise `error` converted to a
 * string.
 *
 * @example
 * ```ts
 * import { getErrorMessage } from "wasp/errors";
 *
 * try {
 *   await createTask({ description: "Buy milk" });
 * } catch (error: unknown) {
 *   window.alert(getErrorMessage(error));
 * }
 * ```
 */
export { getErrorMessage } from "@wasp/lib-sdk-core";
