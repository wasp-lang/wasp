/**
 * Names of the Vite environments Wasp uses.
 *
 * `client` and `ssr` are Vite's built-in environments, see
 * https://vite.dev/guide/api-environment. `ssr` prerenders the client app, so
 * it processes the same code as `client`.
 *
 * `server` is Wasp's own environment for the Node.js server.
 *
 * This module is meant to be shared by all of Wasp's Vite plugins, which is why
 * it sits outside of them. It is internal: plugins import it with a relative
 * path, it is not part of the SDK's public exports.
 */
export const ENVIRONMENT_NAMES = {
  CLIENT: "client",
  SSR: "ssr",
  SERVER: "server",
} as const;
