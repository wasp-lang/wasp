/**
 * In development, the Wasp server sends the errors it reports to the Vite dev
 * server, which pushes them to the browser over its HMR channel. The browser
 * then logs them in its console.
 */

/** Path, relative to the client's base URL, where the Vite dev server receives server errors. */
export const SERVER_ERROR_PATH = "__wasp/server-error";

/** Name of the HMR event that carries a server error to the browser. */
export const SERVER_ERROR_EVENT = "wasp:server-error";

/**
 * Prefix of the browser console message for a server error. Vite uses it to
 * avoid forwarding that message back to the terminal, which already shows it.
 */
export const SERVER_ERROR_CONSOLE_PREFIX = "[Wasp server]";

export interface ServerErrorPayload {
  message: string;
}
