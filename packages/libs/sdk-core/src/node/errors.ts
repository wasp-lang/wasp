/**
 * The type of the function you pass to the `server.onError` field of the app
 * spec. It receives an unexpected server error and the
 * {@link ServerErrorContext} it happened in.
 *
 * @example
 * ```ts
 * import type { ServerOnErrorFn } from "wasp/server";
 *
 * export const onServerError: ServerOnErrorFn = (error, context) => {
 *   errorTracker.capture(error, { source: context.source });
 * };
 * ```
 *
 * @see [Handling unexpected errors](https://wasp.sh/docs/advanced/server-customization/server-config#handling-unexpected-errors)
 */
export type ServerOnErrorFn = (
  error: unknown,
  context: ServerErrorContext,
) => void | Promise<void>;

/**
 * Where an unexpected server error happened.
 *
 * - `"request"`: While handling a request to an Operation, API, or auth route.
 * - `"job"`: In a Job. `jobName` is the name of the Job.
 * - `"webSocket"`: While authenticating a WebSocket connection.
 * - `"oauth"`: During an OAuth login.
 * - `"startup"`: While starting the server, after your code has loaded, e.g.
 *   in `server.setupFn`.
 * - `"dbSeed"`: In a database seed function, when running `wasp db seed`.
 * - `"process"`: Anywhere else in the server, as an uncaught error or an
 *   unhandled promise rejection, e.g. in a timer or while your code loads.
 */
export type ServerErrorContext =
  | { source: "request" }
  | { source: "job"; jobName: string }
  | { source: "webSocket" }
  | { source: "oauth" }
  | { source: "startup" }
  | { source: "dbSeed" }
  | { source: "process" };
