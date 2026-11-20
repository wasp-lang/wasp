// This file must not import the server's config or env. The server installs
// its handlers for uncaught errors through it before any user code loads, and
// the env imports the user's validation schema.

// PRIVATE API (server)
export {
  listenForUncaughtServerErrors,
  registerServerOnErrorFn,
  reportFatalServerError,
  reportUnexpectedServerError,
  sendErrorResponse,
} from "@wasp/lib-sdk-core/node";
