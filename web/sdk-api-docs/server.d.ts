/**
 * Server-side APIs you can use in your Operations, APIs, Jobs, and server
 * setup code.
 *
 * @module
 */

/**
 * Throw an `HttpError` from an Operation or API to send an HTTP error response
 * to the client.
 *
 * Wasp sends the `message` and `data` to the client, and logs everything else.
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
 * @see [Error handling](https://wasp.sh/docs/features/data/operations/queries#error-handling)
 */
export { HttpError } from "@wasp.sh/lib-sdk-core";
